(in-package :veil)



;;; ----------------------------------------------------------------------
;;; Web pages
;;; ----------------------------------------------------------------------

(defclass page ()
  ((page-name            :accessor       page-name            :initarg :page-name)
   (base-url             :accessor       base-url             :initarg :base-url)
   (content-type         :accessor       content-type         :initarg :content-type)
   (request-type         :accessor       request-type         :initarg :request-type)
   (body                 :accessor       body                 :initarg :body)
   (parameter-specs      :accessor       parameter-specs      :initarg :parameter-specs)
   (acceptor             :reader         acceptor)
   (parameter-attributes :reader         parameter-attributes)))

(defmethod initialize-instance :after ((page page) &key)
  (setf (slot-value page 'parameter-attributes)
        (mapcar (lambda (spec)
                  (destructuring-bind (param-name &optional (lisp-type 'string) vspec requiredp)
                      (ensure-list spec)
                    (make-instance 'http-parameter-attributes
                                   :parameter-name param-name
                                   :parameter-key (make-keyword param-name)
                                   :page page
                                   :lisp-type lisp-type
                                   :vfn (cond ((consp vspec) (symbol-function (first vspec)))
                                              ((null vspec) (constantly nil))
                                              (t (symbol-function vspec)))
                                   :vargs (cond ((consp vspec) (rest vspec))
                                                ((null vspec) nil)
                                                (t (list param-name)))
                                   :requiredp requiredp)))
                (parameter-specs page))))

(defgeneric handler (page &key)
  (:documentation "Returns the handler function for a page"))

(defgeneric dispatcher (page)
  (:documentation "Returns the dispatch function for a page"))

(defgeneric register-names (page)
  (:documentation "If the page is of class regex, it returns the
  register names as a list of symbols."))

(defmethod register-names ((page page))
  nil)

(defgeneric parameter-names (page)
  (:documentation "If the page is of class regex, it returns the
  parameter names as a list of symbols."))

(defmethod parameter-names ((page page))
  (mapcar #'parameter-name (parameter-attributes page)))

(defparameter *page* nil
  "This is bound within the body of every page to the page object itself")

(defgeneric page-url (page))

(defmethod page-url ((page page))
  (lambda (registers parameters)
    (declare (ignore registers))
    (with-output-to-string (stream)
      ;; web root
      (princ (web-root (acceptor page)) stream)
      ;; base url
      (princ (base-url page))
      ;; query
      (princ-http-query parameters stream))))

(defun princ-http-query (parameters &optional (stream *standard-output*))
  (iter (for key in parameters by #'cddr)
        (for val in (rest parameters) by #'cddr)
        (princ (if (first-time-p) #\? #\&) stream)
        (when val
          (princ (string-downcase key) stream)
          (princ #\= stream)
          (princ (lisp->urlenc val) stream))))


(defmacro define-page-function (page-name register-names parameter-names)
  `(defun ,page-name ,(append register-names
                       (cons '&key (append parameter-names (list 'fragment))))
     (declare (ignorable fragment))
     (funcall (page-url (find-page ',page-name)) ;;; caution acceptor missing
              (list ,@register-names)
              (list ,@parameter-names))))



;;; ----------------------------------------------------------------------
;;; Page operations
;;; ----------------------------------------------------------------------

;;; -- Find --

(defun find-page (page-name &optional (acceptor (default-acceptor)))
  "Given the page name (a symbol) and an acceptor, return the page object."
  (gethash page-name (pages acceptor)))


;;; -- Register --

(defun register-page (page acceptor)
  "Register a page so that it is served by an acceptor"
  (setf (gethash (page-name page) (pages acceptor)) page)
  (setf (slot-value page 'acceptor) acceptor))

(defun unregister-page (page)
  "Unregister a page so that it is not served by an acceptor"
  (remhash (page-name page) (pages (acceptor page)))
  (slot-makunbound page 'acceptor))


;; -- Build --

(defun build-page (page)
  (funcall (builder page))
  (values))

(defun build-pages (&optional (acceptor (default-acceptor)))
  (iter (for (nil page) in-hashtable (pages acceptor))
        (when (eql (type-of page) 'static-page)
          (build-page page)
          (collect (page-name page)))))


;; -- Publish --

(defun publish-page (page &optional (acceptor (default-acceptor)))
  (setf (assoc-value (slot-value acceptor 'dispatch-table) (page-name page))
        (dispatcher page)))

(defun publish-pages (&optional (acceptor (default-acceptor)))
  (mapc #'(lambda (page)
            (publish-page page acceptor))
        (pages acceptor)))


;; -- Unpublish --

(defun unpublish-page (page)
  (setf (slot-value (acceptor page) 'dispatch-table)
        (remove page (dispatch-table (acceptor page)) :key #'cdr)))

(defun unpublish-pages (&optional (acceptor (default-acceptor)))
  (mapc #'(lambda (page)
            (unpublish-page page))
        (pages acceptor)))


;; -- Published pages --

(defun published-pages (&optional (acceptor (default-acceptor)))
  (iter (for (page-name . nil) in (dispatch-table acceptor))
        (collect page-name)))



;;; ----------------------------------------------------------------------
;;; Dynamic pages
;;; ----------------------------------------------------------------------

(defclass dynamic-page (page)
  ())

(defmethod dispatcher ((page dynamic-page))
  (lambda (request)
    (if (string= (full-base-url page)
                 (script-name request))
        (handler page)
        nil)))

(defmethod handler ((page dynamic-page) &key)
  (lambda ()
    (let ((*page* page)
          (*parameters* (parse-parameters page)))
      (with-output-to-string (*standard-output*)
        (apply (body page) *parameters*)))))

(defmacro define-dynamic-page (page-name (base-url &key (request-type :get)
                                                        (content-type *default-content-type*)
                                                        (page-class 'dynamic-page)
                                                        acceptor)
                               (&rest parameter-specs) &body body)
  (with-gensyms (acc page)
    (let ((parameter-names (collapse parameter-specs)))
      `(let* ((,acc (or ,acceptor (default-acceptor)))
              (,page (make-instance ',page-class
                                    :page-name ',page-name
                                    :base-url ,base-url
                                    :content-type ,content-type
                                    :request-type ,request-type
                                    :parameter-specs ',parameter-specs
                                    :body (lambda (,@parameter-names)
                                            (declare (ignorable ,@parameter-names))
                                            ,@body))))
         (register-page ,page ,acc)
         (publish-page ,page ,acc)
         (define-page-function ,page-name nil ,parameter-names)))))



;;; ----------------------------------------------------------------------
;;; Regex pages
;;; ----------------------------------------------------------------------

(defclass regex-page (dynamic-page)
  ((scanner         :reader scanner)))

(defmethod initialize-instance :after ((page regex-page) &key)
  (setf (slot-value page 'scanner)
        (create-scanner (concatenate 'string
                                     (apply #'concatenate 'string
                                            "^"
                                            (web-root (acceptor page))
                                            (collapse (base-url page) #'second))
                                     "$"))))

(defmethod register-names ((page regex-page))
  (mapcan (lambda (i)
            (if (symbolp i)
                (list i)
                nil))
          (base-url page)))

(defmethod page-url ((page regex-page))
  (lambda (registers parameters)
    (with-output-to-string (stream)
      ;; web root
      (princ (web-root (acceptor page)) stream)
      ;; base url
      (mapc (lambda (item)
              (if (symbolp item)
                  (princ (pop registers) stream)
                  (princ item stream)))
            (collapse (base-url page)))
      ;; query
      (princ-http-query parameters stream))))

(defmethod dispatcher ((page regex-page))
  (lambda (request)
    (multiple-value-bind (match regs) (scan-to-strings (scanner page)
                                                       (script-name request))
      (if match
          (handler page :register-values (coerce regs 'list))
          nil))))

(defmethod handler ((page regex-page) &key register-values)
  (lambda ()
    (let ((*page* page)
          (*parameters* (parse-parameters page)))
      (with-output-to-string (*standard-output*)
        (apply (body page)
               (append *parameters* register-values))))))

(defmacro define-regex-page (page-name (base-url &key (request-type :get)
                                                      (content-type *default-content-type*)
                                                      (page-class 'regex-page)
                                                      acceptor)
                             (&rest parameter-specs) &body body)
  (with-gensyms (acc page)
    (let ((parameter-names (collapse parameter-specs))
          (register-names (collapse base-url)))
      `(let* ((,acc (or ,acceptor (default-acceptor)))
              (,page (make-instance ',page-class
                                    :page-name ',page-name
                                    :base-url ',base-url
                                    :content-type ,content-type
                                    :request-type ,request-type
                                    :body (lambda (,@parameter-names ,@register-names)
                                            (declare (ignorable ,@parameter-names ,@register-names))
                                            ,@body)
                                    :parameter-specs ',parameter-specs)))
         (register-page ,page ,acc)
         (publish-page ,page ,acc)
         (define-page-function ,page-name ,register-names ,parameter-names)))))



;;; ----------------------------------------------------------------------
;;; Static pages
;;; ----------------------------------------------------------------------

(defclass static-page (page)
  ((location :accessor location :initarg :location)))

(defmethod initialize-instance :after ((page static-page) &key)
  (unless (location page)
    (setf (location page) (static-page-pathname page))))

(defmethod dispatcher ((page static-page))
  (lambda (request)
    (if (string= (full-base-url page)
                 (script-name request))
        (handle-static-file (location page) (content-type page))
        nil)))

(defgeneric builder (page)
  (:documentation "Return a function which, when called, writes the
  body of the page to the path of the page"))

(defmethod builder ((page static-page))
  (lambda ()
    (ensure-directories-exist (location page))
    (with-open-file (stream (location page)
                            :element-type 'base-char
                            :direction :output
                            :if-does-not-exist :create
                            :if-exists :supersede)
      (let ((*standard-output* stream))
        (funcall (body page))))))

(defmacro define-static-page (page-name (base-url &key location
                                                       (request-type :get)
                                                       (content-type *default-content-type*)
                                                       (page-class 'static-page)
                                                       acceptor)
                              (&rest parameter-specs)
                              &body body)
  (with-gensyms (acc page)
    (let ((parameter-names (collapse parameter-specs)))
      `(let* ((,acc (or ,acceptor (default-acceptor)))
              (,page (make-instance ',page-class
                                    :page-name ',page-name
                                    :base-url ,base-url
                                    :content-type ,content-type
                                    :request-type ,request-type
                                    :location ,location
                                    :body (lambda ()
                                            ,@body)
                                    :parameter-specs ',parameter-specs)))
         (register-page ,page ,acc)
         (publish-page ,page ,acc)
         (define-page-function ,page-name nil ,parameter-names)))))



;;; ----------------------------------------------------------------------
;;; Auxiliary
;;; ----------------------------------------------------------------------

(defun collapse (list &optional (fn #'first))
  (mapcar (lambda (item)
            (if (listp item)
                (funcall fn item)
                item))
          list))

(defun full-base-url (page)
  (concatenate 'string (web-root (acceptor page)) (base-url page)))

(defun static-page-pathname (page)
  (let ((relative-path
         (if (ends-with #\/ (base-url page))
             (make-pathname :directory (if (emptyp (base-url page))
                                           `(:relative ,@(split "/" (base-url page)))
                                           nil)
                            :name "index"
                            :type "html")
             (cl-fad:pathname-as-file
              (make-pathname :directory `(:relative ,@(split "/" (base-url page))))))))
    (merge-pathnames relative-path (doc-root (acceptor page)))))



;; (defmacro define-page-function (page)
;;   (let ((page-name (page-name page))
;;         (parameter-names (parameter-names page))
;;         (register-names (register-names page)))
;;     `(defun ,page-name ,(append register-names
;;                          (cons '&key (append parameter-names (list 'fragment))))
;;        (declare (ignorable fragment))
;;        (concatenate 'string
;;                     ,(web-root (acceptor page))
;;                     (apply #'concatenate 'string
;;                            (substitute-from-list-if #'symbolp
;;                                                     ',(collapse (base-url page))
;;                                                     (list ,@register-names)))
;;                     (make-query-string (mapcan (lambda (name value)
;;                                                  (when value
;;                                                    (list (cons name value))))
;;                                                ',parameter-names (list ,@parameter-names)))))))

;; (defmacro define-page-fn (page)
;;   (let ((page-name (page-name page))
;;         (parameter-names (mapcar #'name (parameter-attributes page))))
;;     `(defun ,page-name ,(cons '&key (append parameter-names (list 'fragment)))
;;        (declare (ignorable fragment))
;;        (concatenate 'string
;;                     ,(web-root (acceptor page))
;;                     ,(base-url page)
;;                     (make-query-string (mapcan (lambda (name value)
;;                                                  (when value
;;                                                    (list (cons name value))))
;;                                                ',parameter-names (list ,@parameter-names)))))))

;; (defmacro define-page-fn (page-name acceptor &optional arguments)
;;   (with-gensyms (page param-value-alist)
;;     `(defun ,page-name ,(cons '&key (append arguments (list 'fragment)))
;;        (declare (ignorable ,@arguments fragment))
;;        (let ((,page (find-page ',page-name ,acceptor))
;;              (,param-value-alist (iter (for arg in ',arguments)
;;                                        (for val in (list ,@arguments))
;;                                        (when val
;;                                          (collect (cons arg val))))))
;;          (concatenate 'string
;;                       (web-root (acceptor ,page))
;;                       (base-url ,page)
;;                       (make-query-string ,param-value-alist))))))

;; (defmacro define-regex-page-fn (page-name acceptor &optional arguments)
;;   (with-gensyms (page param-value-alist)
;;     `(defun ,page-name ,(append (register-names page)
;;                          (cons '&key (append arguments (list 'fragment))))
;;        (declare (ignorable ,@arguments fragment))
;;        (let ((,page (find-page ',page-name ,acceptor))
;;              (,param-value-alist (iter (for arg in ',arguments)
;;                                        (for val in (list ,@arguments))
;;                                        (when val
;;                                          (collect (cons arg val))))))
;;          (concatenate 'string
;;                       (web-root (acceptor ,page))
;;                       (apply #'concatenate
;;                              'string
;;                              (substitute-from-list-if #'symbolp
;;                                                       (collapse (base-url page))
;;                                                       (list ,@(register-names page))))
;;                       (make-query-string ,param-value-alist))))))


(defun substitute-from-list-if (predicate base-list substitutions-list)
  (mapcar (lambda (item)
            (if (funcall predicate item)
                (pop substitutions-list)
                item))
          base-list))
