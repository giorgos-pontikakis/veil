(in-package :veil)



;;; ----------------------------------------------------------------------
;;; Parameter attributes
;;; ----------------------------------------------------------------------

(defclass http-parameter-attributes ()
  ((param-name :accessor param-name :initarg :param-name)
   (param-key  :accessor param-key  :initarg :param-key)
   (page       :accessor page       :initarg :page)
   (lisp-type  :accessor lisp-type  :initarg :lisp-type)
   (vfn        :accessor vfn        :initarg :vfn)
   (vargs      :accessor vargs      :initarg :vargs)
   (requiredp  :accessor requiredp  :initarg :requiredp)))



;;; ----------------------------------------------------------------------
;;; Web pages
;;; ----------------------------------------------------------------------

(defclass page ()
  ((page-name            :accessor page-name            :initarg :page-name)
   (base-url             :accessor base-url             :initarg :base-url)
   (content-type         :accessor content-type         :initarg :content-type)
   (request-type         :accessor request-type         :initarg :request-type)
   (acceptor             :accessor acceptor             :initarg :acceptor)
   (body                 :accessor body                 :initarg :body)
   (parameter-attributes :reader   parameter-attributes)))

(defmethod initialize-instance :after ((page page) &key)
  (setf (slot-value page 'parameter-attributes)
        (mapcar (lambda (spec)
                  (destructuring-bind (param-name &optional (lisp-type 'string) vspec requiredp)
                      (ensure-list spec)
                    (make-instance 'http-parameter-attributes
                                   :param-name param-name
                                   :param-key (make-keyword param-name)
                                   :page page
                                   :lisp-type lisp-type
                                   :vfn (cond ((consp vspec) (symbol-function (first vspec)))
                                              ((null vspec) '(constantly nil))
                                              (t (symbol-function vspec)))
                                   :vargs (cond ((consp vspec) (rest vspec))
                                                ((null vspec) nil)
                                                (t (list param-name)))
                                   :requiredp requiredp)))
                specs)))

(defgeneric publisher (page)
  (:documentation "Return a function which, when called, returns the
  dispatcher for a page"))

(defgeneric find-page (page-name acceptor)
  (:documentation "Take the page name (a symbol) and an
acceptor. Return the page object. "))

(defmethod find-page (page-name (acceptor veil-acceptor))
  (gethash page-name (pages acceptor)))

(defun register-page (page acceptor)
  "Register a page so that it is served by an acceptor"
  (setf (acceptor page) acceptor)
  (setf (gethash (page-name page) (pages acceptor)) page))

(defun unregister-page (page-name acceptor)
  "Unregister a page so that it is not served by an acceptor"
  (let ((page (find-page page-name acceptor)))
    (if page
        (remhash (page-name page) (pages acceptor))
        (error "Page ~A not found." page-name))))

(defparameter *page* nil)
(defparameter *parameters* nil)

(defmacro define-page-fn (page-name acceptor &optional arguments)
  (with-gensyms (page param-value-alist)
    `(defun ,page-name ,(cons '&key (append arguments (list 'fragment)))
       (declare (ignorable ,@arguments fragment))
       (let ((,page (find-page ',page-name ,acceptor))
             (,param-value-alist (iter (for arg in ',arguments)
                                       (for val in (list ,@arguments))
                                       (when val
                                         (collect (cons arg val))))))
         (concatenate 'string
                      (web-root (acceptor ,page))
                      (base-url ,page)
                      (make-query-string ,param-value-alist))))))

(defmacro define-regex-page-fn (page-name acceptor registers &optional arguments)
  (with-gensyms (page param-value-alist)
    `(defun ,page-name ,(append registers
                                (cons '&key (append arguments (list 'fragment))))
       (declare (ignorable ,@arguments fragment))
       (let ((,page (find-page ',page-name ,acceptor))
             (,param-value-alist (iter (for arg in ',arguments)
                                       (for val in (list ,@arguments))
                                       (when val
                                         (collect (cons arg val))))))
         (concatenate 'string
                      (web-root (acceptor ,page))
                      (apply (url-fn ,page) (mapcar #'lisp->urlenc (list ,@registers)))
                      (make-query-string ,param-value-alist))))))



;;; ----------------------------------------------------------------------
;;; Dynamic and regex pages
;;; ----------------------------------------------------------------------

(defgeneric handler (page &key)
  (:documentation "Return a function which, when called, returns the
  handler for a page of class dynamic-page or its subclasses"))


;;; --- Dynamic pages ---

(defclass dynamic-page (page)
  ())

(defmethod publisher ((page dynamic-page))
  #'(lambda ()
      (setf (gethash (page-name page)
                     (dispatch-table (acceptor page)))
            (lambda (request)
              (if (string= (full-base-url page)
                           (script-name request))
                  (handler page)
                  nil)))))

(defmethod handler ((page dynamic-page) &key)
  #'(lambda ()
      (let ((*page* page)
            (*parameters* (parse-parameters page)))
        (with-output-to-string (*standard-output*)
          (apply (body page) *parameters*)))))

(defmacro define-dynamic-page (page-name (base-url &key (request-type :get)
                                                        (content-type *default-content-type*)
                                                        acceptor)
                               (&rest param-specs) &body body)
  (with-gensyms (page parameter-attributes webapp)
    (let ((parameter-names (build-parameter-names param-specs)))
      `(let ((,page
              (make-instance 'dynamic-page
                             :page-name ',page-name
                             :base-url ,base-url
                             :content-type ,content-type
                             :request-type ,request-type
                             :body (lambda (,@parameter-names)
                                     (declare (ignorable ,@parameter-names))
                                     ,@body))))
         (register-page ,page (or ,acceptor (package-webapp)))
         (define-page-fn ,page-name (or ,webapp (package-webapp)) ,parameter-names)
         (publish-page ',page-name (or ,webapp (package-webapp)))))))



;;; --- Regex pages ---

(defclass regex-page (dynamic-page)
  ((scanner         :reader scanner)
   (register-names  :accessor register-names  :initarg :register-names)
   (url-fn          :accessor url-fn          :initarg :url-fn)))

(defmethod initialize-instance :after ((page regex-page))
  (setf (slot-value page 'scanner)
        (create-scanner (concatenate 'string
                                     "^"
                                     (web-root (acceptor page))
                                     ,@(mapcar (lambda (item)
                                                 (if (listp item)
                                                     (second item)
                                                     item))
                                               base-url)
                                     "$"))))

(defmethod publisher ((page regex-page))
  (lambda ()
    (setf (gethash (page-name page)
                   (dispatch-table (webapp page)))
          (lambda (request)
            (multiple-value-bind (match regs) (scan-to-strings (scanner page)
                                                               (script-name request))
              (if match
                  (handler page :register-values (coerce regs 'list))
                  nil))))))

(defmethod handler ((page regex-page) &key register-values)
  (lambda ()
    (let ((*page* page)
          (*parameters* (parse-parameters page)))
      (with-output-to-string (*standard-output*)
        (apply (body page)
               (append *parameters* register-values))))))

(defmacro define-regex-page (page-name (base-url &key
                                                   (request-type :get)
                                                   (content-type *default-content-type*)
                                                   webapp-name)
                             (&rest param-specs) &body body)
  (with-gensyms (page parameter-attributes webapp)
    (let ((param-names (build-parameter-names param-specs))
          (register-names (mapcan (lambda (item)
                                    (if (listp item)
                                        (list (first item))
                                        nil))
                                  base-url)))
      `(let* ((,webapp (find-webapp ',webapp-name))
              (,page (make-instance 'regex-page
                                    :page-name ',page-name
                                    :base-url ',base-url
                                    :register-names ',register-names
                                    :url-fn (lambda ,register-names
                                              (concatenate 'string ,@(mapcar (lambda (item)
                                                                               (if (listp item)
                                                                                   (first item)
                                                                                   item))
                                                                             base-url)))
                                    :request-type ,request-type
                                    :content-type ,content-type
                                    :body (lambda (,@param-names ,@register-names)
                                            (declare (ignorable ,@param-names ,@register-names))
                                            ,@body))))
         (register-page ,page (or ,webapp (package-webapp)))
         (define-regex-page-fn ,page-name (or ,webapp (package-webapp)) ,register-names ,param-names)
         (publish-page ',page-name (or ,webapp (package-webapp)))))))



;;; ----------------------------------------------------------------------
;;; Static pages
;;; ----------------------------------------------------------------------

(defclass static-page (page)
  ((location :accessor location :initarg :location)))

(defmethod initialize-instance :after ((page static-page))
  (unless (location page)
    (setf (location page) (static-page-pathname page))))

(defmethod publisher ((page static-page))
  (lambda ()
    (setf (gethash (page-name page)
                   (dispatch-table (webapp page)))
          (lambda (request)
            (if (string= (full-base-url page)
                         (script-name request))
                (handle-static-file (location page) (content-type page))
                nil)))))

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

(defmacro define-static-page (page-name (base-url &key (request-type :get)
                                                  (content-type *default-content-type*)
                                                  webapp-name
                                                  location)
                              (&rest param-specs)
                              &body body)
  (with-gensyms (page parameter-attributes webapp)
    (let ((parameter-names (build-parameter-names param-specs)))
      `(let* ((,webapp (find-webapp ,webapp-name))
              (,page (make-instance 'static-page
                                    :page-name ',page-name
                                    :base-url ,base-url
                                    :location ,location
                                    :request-type ,request-type
                                    :content-type ,content-type
                                    :body (lambda ()
                                            ,@body))))
         (register-page ,page (or ,webapp (package-webapp)))
         (define-page-fn ,page-name (or ,webapp (package-webapp)) ,parameter-names)
         (publish-page ',page-name (or ,webapp (package-webapp)))))))



;; ----------------------------------------------------------------------
;; Build, Publish and Unpublish
;; ----------------------------------------------------------------------

;; -- Build --

(defun build-page (page)
  (funcall (builder page))
  (values))

(defun build-pages (&optional (webapp (package-webapp)))
  (let ((app (ensure-webapp webapp)))
    (iter (for (nil page) in-hashtable (pages app))
          (when (eql (type-of page) 'static-page)
            (funcall (builder page))
            (collect (page-name page))))))



;; -- Publish --

(defun publish-page (page-name &optional (webapp (package-webapp)))
  (funcall (publisher (find-page page-name webapp))))

(defun publish-pages (&optional (webapp (package-webapp)))
  (iter (for (nil page) in-hashtable (pages webapp))
        (unless (eql (type-of page) 'static-page)
          (funcall (publisher page))
          (collect (page-name page)))))


;; -- Unpublish --

;; Not implemented yet.



;; -- Published pages --

(defun published-pages (&optional (webapp (package-webapp)))
  (iter (for (page-name nil) in-hashtable (dispatch-table (ensure-webapp webapp)))
        (collect page-name)))



;;; ----------------------------------------------------------------------
;;; Auxiliary
;;; ----------------------------------------------------------------------

(defun build-parameter-names (spec)
  (mapcar #'first (mapcar #'ensure-list spec)))

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
    (merge-pathnames relative-path (doc-root (package-webapp)))))
