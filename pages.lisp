(in-package :veil)



;;; ----------------------------------------------------------------------
;;; Web pages
;;; ----------------------------------------------------------------------

(defclass page ()
  ((page-name            :accessor page-name            :initarg :page-name)
   (base-url             :accessor base-url             :initarg :base-url)
   (content-type         :accessor content-type         :initarg :content-type)
   (request-type         :accessor request-type         :initarg :request-type)
   (body                 :accessor body                 :initarg :body)
   (parameter-specs      :accessor parameter-specs      :initarg :parameter-specs)
   (acceptor             :reader   acceptor)
   (parameter-attributes :reader   parameter-attributes)))

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

(defgeneric register-names (obj)
  (:documentation "If the page is of class regex, it returns the
  register names as a list of symbols."))

(defmethod register-names ((obj t))
  nil)

(defmethod register-names ((obj list))
  (mapcan (lambda (i)
            (if (and (listp i) (symbolp (first i)))
                (list (first i))
                nil))
          obj))

(defgeneric parameter-names (page)
  (:documentation "If the page is of class regex, it returns the
  parameter names as a list of symbols."))

(defmethod parameter-names ((obj page))
  (mapcar #'parameter-name (parameter-attributes obj)))

(defmethod parameter-names ((obj list))
  (mapcar (lambda (item)
            (if (listp item)
                (first item)
                item))
          obj))

(defgeneric page-url (page)
  (:documentation "Produces a function which receives registers and
  parameters and returns the url of a page"))

(defmethod page-url ((page page))
  (lambda (registers parameters)
    (declare (ignore registers))
    (with-output-to-string (stream)
      ;; web root
      (princ (web-root (acceptor page)) stream)
      ;; base url
      (princ (base-url page) stream)
      ;; query
      (princ-http-query page parameters stream))))

(defparameter *page* nil
  "This is bound within the body of every page to the page object itself")


;;; ----------------------------------------------------------------------
;;; Page operations
;;; ----------------------------------------------------------------------

;;; -- Find --

(defun find-page (page-name &optional (acceptor (default-acceptor)))
  "Given the page name (a symbol) and an acceptor, return the page object."
  (gethash page-name (pages acceptor)))


;;; -- Register --

(defgeneric register-page (page acceptor)
  (:documentation "Register a page so that it is served by an acceptor"))

(defmethod register-page ((page page) acceptor)
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
;;; DYNAMIC pages
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



;;; ----------------------------------------------------------------------
;;; REGEX pages
;;; ----------------------------------------------------------------------

(defclass regex-page (dynamic-page)
  ((scanner :reader scanner)))

(defmethod register-page :after ((page regex-page) acceptor)
  (let ((scanner (with-output-to-string (stream)
                   ;; web root
                   (princ "^" stream)
                   (princ (web-root (acceptor page)) stream)
                   ;; base url
                   (mapc (lambda (item)
                           (cond ((stringp item)
                                  (princ item stream))
                                 ((listp item)
                                  (princ (second item) stream))
                                 (t
                                  (error "Malformed base-url for regex page scanner"))))
                         (base-url page))
                   ;; end
                   (princ "$" stream))))
    (setf (slot-value page 'scanner)
          (create-scanner scanner))))

(defmethod register-names ((page regex-page))
  (register-names (base-url page)))

(defmethod page-url ((page regex-page))
  (lambda (registers parameters)
    (with-output-to-string (stream)
      ;; web root
      (princ (web-root (acceptor page)) stream)
      ;; base url
      (mapc (lambda (item)
              (cond ((stringp item)
                     (princ item stream))
                    ((and (listp item) (symbolp (first item)))
                     (princ (pop registers) stream))
                    (t
                     (error "Malformed base-url for regex page"))))
            (base-url page))
      ;; query
      (princ-http-query page parameters stream))))

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



;;; ----------------------------------------------------------------------
;;; STATIC pages
;;; ----------------------------------------------------------------------

(defclass static-page (page)
  ((location :accessor location :initarg :location)))

(defmethod register-page :after ((page static-page) acceptor)
  (unless (slot-boundp page 'location)
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



;;; ------------------------------------------------------------
;;; DEFPAGE macro
;;; ------------------------------------------------------------

(defmacro defpage (page-class page-name (base-url &rest initargs)
                   (&rest parameter-specs) &body body)
  (with-gensyms (acceptor page)
    (let ((parameter-names (parameter-names parameter-specs))
          (register-names (register-names base-url))
          (acceptor-name (getf initargs :acceptor-name)))
      `(let* ((,acceptor (or (find-acceptor ',acceptor-name) (default-acceptor)))
              (,page (make-instance ',page-class
                                    :page-name ',page-name
                                    :base-url ',base-url
                                    :content-type ,(getf initargs :content-type
                                                         *default-content-type*)
                                    :request-type ,(getf initargs :request-type :get)
                                    :parameter-specs ',parameter-specs
                                    :body (lambda (,@parameter-names ,@register-names)
                                            (declare (ignorable ,@parameter-names ,@register-names))
                                            ,@body)
                                    ,@(remove-from-plist initargs
                                                         :content-type :request-type
                                                         :acceptor-name))))
         (register-page ,page ,acceptor)
         (publish-page ,page ,acceptor)
         (define-page-function ,acceptor-name ,page-name ,register-names ,parameter-names)))))

(defmacro define-page-function (acceptor-name page-name register-names parameter-names)
  `(defun ,page-name ,(append register-names
                       (cons '&key (append parameter-names (list 'fragment))))
     (declare (ignorable fragment))
     (funcall (page-url (find-page ',page-name
                                   (or (find-acceptor ',acceptor-name) (default-acceptor))))
              (list ,@register-names)
              (list ,@parameter-names))))



;;; ----------------------------------------------------------------------
;;; Auxiliary
;;; ----------------------------------------------------------------------

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




(defun substitute-from-list-if (predicate base-list substitutions-list)
  (mapcar (lambda (item)
            (if (funcall predicate item)
                (pop substitutions-list)
                item))
          base-list))
