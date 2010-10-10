(in-package :veil)

(declaim (optimize (speed 0) (debug 3)))

;;; ----------------------------------------------------------------------
;;; Web pages
;;; ----------------------------------------------------------------------

(defclass page ()
  ((name         :accessor name         :initarg :name)
   (key          :accessor key          :initarg :key)
   (webapp       :reader   webapp)
   (base-url     :accessor base-url     :initarg :base-url)
   (content-type :accessor content-type :initarg :content-type)
   (body         :accessor body         :initarg :body)
   (request-type :accessor request-type :initarg :request-type)
   (parameters   :accessor parameters   :initarg :parameters)))

(defgeneric publisher (page)
  (:documentation "Return a function which, when called, returns the
  dispatcher for a page"))

(defun find-page (name &optional webapp)
  "Take the page name (a symbol) and a webapp designator (symbol or
object). Return the page object. "
  (gethash name (pages (ensure-webapp webapp))))

(defun register-page (page &optional webapp)
  "Add a page to a webapp's pages"
  (let ((app (ensure-webapp webapp)))
    (setf (slot-value page 'webapp) app)
    (setf (gethash (name page) (pages app))
          page)))

(defun unregister-page (page-name &optional webapp)
  "Remove a page from a webapp's pages"
  (let ((page (find-page page-name webapp)))
    (if page
        (remhash (name page) (pages (webapp page)))
        (error "Page ~A not found." page-name))))

(defun full-url (page-name &optional webapp)
  (let ((page (find-page page-name webapp)))
    (if page
        (concatenate 'string (webroot (webapp page)) (base-url page))
        (error "Page ~A not found." page-name))))

(defparameter *page* nil)

(defmacro define-page-fn (page-name &optional arguments)
  (with-gensyms (page param-value-alist)
    `(defun ,page-name ,(cons '&key (append arguments (list 'fragment)))
       (declare (ignorable fragment))
       (let ((,page (find-page ',page-name))
             (,param-value-alist (iter (for arg in ',arguments)
                                       (for val in (list ,@arguments))
                                       (when val
                                         (collect (cons arg val))))))
         (concatenate 'string
                      (webroot (webapp ,page))
                      (base-url ,page)
                      (make-query-string ,param-value-alist))))))



;;; ----------------------------------------------------------------------
;;; Dynamic and regex pages
;;; ----------------------------------------------------------------------

(defgeneric handler (page &key)
  (:documentation "Return a function which, when called, returns the
  handler for a page of class dynamic-page or its subclasses"))

(defun build-parameter-list (page specs)
  (mapcar (lambda (spec)
            (destructuring-bind (name &optional
                                      (lisp-type 'string)
                                      vspec
                                      requiredp) (ensure-list spec)
              `(make-instance 'http-parameter
                              :name ',name
                              :page ,page
                              :key (make-keyword ',name)
                              :lisp-type ',lisp-type
                              :vfn ,(cond ((consp vspec) `(symbol-function ',(first vspec)))
                                          ((null vspec) '(constantly t))
                                          (t `(symbol-function ',vspec)))
                              :vargs ',(cond ((consp vspec) (rest vspec))
                                             ((null vspec) nil)
                                             (t (list name)))
                              :requiredp ',requiredp)))
          specs))


(defun build-parameter-names (spec)
  (mapcar #'first (mapcar #'ensure-list spec)))



;;; --- Dynamic pages ---

(defclass dynamic-page (page)
  ())

(defmethod publisher ((page dynamic-page))
  #'(lambda ()
      (setf (gethash (name page)
                     (dispatch-table (webapp page)))
            (lambda (request)
              (if (string-equal (full-url (name page))
                                (script-name request))
                  (progn
                    (set-parameters page)
                    (handler page))
                  nil)))))

(defmethod handler ((page dynamic-page) &key)
  #'(lambda ()
      (let ((*page* page))
        (declare (special *page*))
        (with-output-to-string (*standard-output*)
          (apply (body page) (parameters page))))))


(defmacro define-dynamic-page (name (base-url &key
                                              (request-type :get)
                                              (content-type *default-content-type*)
                                              webapp)
                               (&rest param-specs) &body body)
  (with-gensyms (page parameters)
    (let ((parameter-names (build-parameter-names param-specs)))
      `(let* ((,page (make-instance 'dynamic-page
                                    :name ',name
                                    :key (make-keyword ',name)
                                    :base-url ,base-url
                                    :request-type ,request-type
                                    :content-type ,content-type
                                    :body (lambda (,@parameter-names)
                                            ,@body)))
              (,parameters (list ,@(build-parameter-list page param-specs))))
         (register-page ,page
                        (or ,webapp *webapp*))
         (setf (parameters ,page) ,parameters)
         (define-page-fn ,name ,parameter-names)
         (publish-page ',name)))))



;;; --- Regex pages ---

(defclass regex-page (dynamic-page)
  ((scanner :accessor scanner :initarg :scanner)))

(defmethod publisher ((page regex-page))
  (lambda ()
    (setf (gethash (name page)
                   (dispatch-table (webapp page)))
          (lambda (request)
            (multiple-value-bind (match regs) (scan-to-strings (scanner page)
                                                               (script-name request))
              (if match
                  (progn
                    (set-parameters page)
                    (handler page :register-values (coerce regs 'list)))
                  nil))))))

(defmethod handler ((page regex-page) &key register-values)
  #'(lambda ()
      (let ((*page* page))
        (declare (special *page*))
        (with-output-to-string (*standard-output*)
          (apply (body page)
                 (append (parameters page) register-values))))))

(defmacro define-regex-page (name (base-url &key
                                            (request-type :get)
                                            (content-type *default-content-type*)
                                            registers
                                            webapp)
                             (&rest param-specs) &body body)
  (with-gensyms (page parameters)
    (let ((parameter-names (build-parameter-names param-specs)))
      `(let* ((,page (make-instance 'regex-page
                                    :name ',name
                                    :key (make-keyword ',name)
                                    :base-url ,base-url
                                    :request-type ,request-type
                                    :content-type ,content-type
                                    :body (lambda (,@parameter-names ,@registers)
                                            ,@body)))
              (,parameters (list ,@(build-parameter-list page param-specs))))
         (register-page ,page
                        (or ,webapp *webapp*))
         (setf (parameters ,page) ,parameters)
         (setf (scanner ,page)
               (create-scanner (concatenate 'string
                                            "^"
                                            (full-url (name ,page)))))
         (define-page-fn ,name ,parameter-names)
         (publish-page ',name)))))



;;; ----------------------------------------------------------------------
;;; Static pages
;;; ----------------------------------------------------------------------

(defclass static-page (page)
  ((path         :accessor path         :initarg :path)))

(defmethod publisher ((page static-page))
  (lambda ()
    (setf (gethash (name page)
                   (dispatch-table (webapp page)))
          (lambda (request)
            (if (string-equal (full-url (name page))
                              (script-name request))
                (handle-static-file (path page) (content-type page))
                nil)))))

(defgeneric builder (page)
  (:documentation "Return a function which, when called, writes the
  body of the page to the path of the page"))

(defmethod builder ((page static-page))
  (lambda ()
    (ensure-directories-exist (pathname page))
    (with-open-file (stream (pathname page)
                            :element-type 'base-char
                            :direction :output
                            :if-does-not-exist :create
                            :if-exists :supersede)
      (let ((*standard-output* stream))
        (render (body page))))))

(defmacro define-static-page (name (base-url &key
                                             (request-type :get)
                                             (content-type *default-content-type*)
                                             webapp
                                             path)
                              (&rest param-specs)
                              &body body)
  (with-gensyms (page parameters)
    (let ((parameter-names (build-parameter-names param-specs)))
      `(let* ((,page (make-instance 'static-page
                                    :name ',name
                                    :key (make-keyword ',name)
                                    :base-url ,base-url
                                    :path ,path
                                    :request-type ,request-type
                                    :content-type ,content-type
                                    :body (lambda ()
                                            ,@body)))
              (,parameters (list ,@(build-parameter-list page param-specs))))
         (setf (parameters ,page) ,parameters)
         (register-page ,page
                        (or ,webapp *webapp*))
         (unless (path ,page)
           (setf (path ,page) (static-page-pathname ,page)))
         (define-page-fn ,name ,parameter-names)
         (publish-page ',name)))))

(defun static-page-pathname (page)
  (let* ((split-base-url (split "/" (base-url page)))
         (static-directory (or (butlast split-base-url) (list "")))
         (static-filename (lastcar split-base-url))
         (webapp (webapp page)))
    (make-pathname* :file static-filename
                    :dir (cons (root-path webapp)
                               (cons (static-path webapp)
                                     static-directory)))))

;; ----------------------------------------------------------------------
;; Build, Publish and Unpublish
;; ----------------------------------------------------------------------

;; -- Build --

;; (defgeneric %build-page (page))

;; (defmethod %build-page ((page static-page))
;;   (funcall (builder page) (static-page-pathname page)))

;; (defmethod %build-page ((page dynamic-page))
;;   (values))

;; (defmethod %build-page ((page external-page))
;;   (values))

(defun build-page (page-name &optional webapp)
  (funcall (builder (find-page page-name (ensure-webapp webapp)))))

(defun build-pages (&optional webapp)
  (iter (for page in (pages (ensure-webapp webapp)))
        (build-page page)
        (collect (name page))))


;; -- Publish --

;; (defgeneric %publish-page (page))

;; (defmethod %publish-page ((page static-page))
;;   (funcall (publisher page) (static-page-pathname page)))

;; (defmethod %publish-page ((page dynamic-page))
;;   (funcall (publisher page)))

;; (defmethod %publish-page ((page external-page))
;;   (values))

(defun publish-page (page-name &optional webapp)
  (funcall (publisher (find-page page-name (ensure-webapp webapp)))))

(defun publish-pages (&optional webapp)
  (iter (for (nil page) in-hashtable (pages (ensure-webapp webapp)))
        (publish-page page)
        (collect (name page))))


;; -- Unpublish --

;; Not implemented yet.



;; -- Published pages --

(defun published-pages (&optional webapp)
  (iter (for (name nil) in-hashtable (dispatch-table (ensure-webapp webapp)))
        (collect name)))