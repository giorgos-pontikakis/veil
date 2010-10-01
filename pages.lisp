(in-package :veil)

(declaim (optimize (speed 0) (debug 3)))

;;; ----------------------------------------------------------------------
;;; Web pages
;;; ----------------------------------------------------------------------

(defclass page ()
  ((name     :accessor name     :initarg :name)
   (key      :accessor key      :initarg :key)
   (webapp   :reader   webapp)
   (base-url :accessor base-url :initarg :base-url)))


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
;;; Dynamic pages
;;; ----------------------------------------------------------------------

(defclass dynamic-page (page)
  ((request-type :accessor request-type :initarg :request-type)
   (handler      :accessor handler      :initarg :handler)
   (parameters   :accessor parameters   :initarg :parameters)
   (tests        :accessor tests        :initarg :tests)
   (body         :accessor body         :initarg :body)))

(defmethod publisher ((page dynamic-page))
  #'(lambda ()
      (setf (gethash (name page)
                     (dispatch-table (webapp page)))
            ;; this is the dispatcher
            #'(lambda (request)
                (if (string-equal (full-url (name page))
                                  (script-name request))
                    (handler page)
                    nil)))))

(defmethod handler ((page dynamic-page))
  #'(lambda ()
      (let ((*page* page))
        (declare (special *page*))
        (set-parameters page)
        (let ((output (with-output-to-string (*standard-output*)
                        (apply (body page) (parameters page)))))
          output))))

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

(defmacro define-dynamic-page (name (base-url &key
                                              (request-type :get)
                                              webapp)
                               (&rest param-specs) &body body)
  (with-gensyms (page parameters)
    (let ((parameter-names (build-parameter-names param-specs)))
      `(let* ((,page (make-instance 'dynamic-page
                                    :name ',name
                                    :key (make-keyword ',name)
                                    :base-url ,base-url
                                    :request-type ,request-type
                                    :body (lambda (,@parameter-names)
                                            ,@body)))
              (,parameters (list ,@(build-parameter-list page param-specs))))
         (setf (parameters ,page) ,parameters)
         (register-page
          ,page
          (or ,webapp *webapp*))
         (define-page-fn ,name ,parameter-names)
         (publish-page ',name)))))



;;; ----------------------------------------------------------------------
;;; External pages
;;; ----------------------------------------------------------------------

(defclass external-page (page)
  ((request-type :accessor request-type :initarg :request-type)))



;;; ----------------------------------------------------------------------
;;; Static pages
;;; ----------------------------------------------------------------------

(defclass static-page (page)
  ((content-type :accessor content-type :initarg :content-type)
   (path         :accessor path         :initarg :path)
   (builder      :accessor builder      :initarg :builder)
   (publisher    :accessor publisher    :initarg :publisher)))



;; ----------------------------------------------------------------------
;; Build, Publish and Unpublish
;; ----------------------------------------------------------------------

(defun static-page-pathname (page)
  (let* ((split-base-url (split "/" (base-url page)))
         (static-directory (or (butlast split-base-url) (list "")))
         (static-filename (lastcar split-base-url))
         (webapp (webapp page)))
    (make-pathname* :file static-filename
                    :dir (cons (root-path webapp)
                               (cons (static-path webapp)
                                     static-directory)))))


;; -- Build --

(defgeneric %build-page (page))

(defmethod %build-page ((page static-page))
  (funcall (builder page) (static-page-pathname page)))

(defmethod %build-page ((page dynamic-page))
  (values))

(defmethod %build-page ((page external-page))
  (values))

(defun build-page (page-name &optional webapp)
  (%build-page (find-page page-name (ensure-webapp webapp))))

(defun build-pages (&optional webapp)
  (iter (for page in (pages (ensure-webapp webapp)))
        (%build-page page)
        (collect (name page))))


;; -- Publish --

(defgeneric %publish-page (page))

(defmethod %publish-page ((page static-page))
  (funcall (publisher page) (static-page-pathname page)))

(defmethod %publish-page ((page dynamic-page))
  (funcall (publisher page)))

(defmethod %publish-page ((page external-page))
  (values))

(defun publish-page (page-name &optional webapp)
  (%publish-page (find-page page-name (ensure-webapp webapp))))

(defun publish-pages (&optional webapp)
  (iter (for (nil page) in-hashtable (pages (ensure-webapp webapp)))
        (%publish-page page)
        (collect (name page))))


;; -- Unpublish --

;; Not implemented yet.



;; -- Published pages --

(defun published-pages (&optional webapp)
  (iter (for (name nil) in-hashtable (dispatch-table (ensure-webapp webapp)))
        (collect name)))