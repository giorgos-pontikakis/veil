(in-package :veil)

(declaim (optimize (speed 0) (debug 3)))

;;; ----------------------------------------------------------------------
;;; Web pages
;;; ----------------------------------------------------------------------

(defclass page ()
  ((name     :accessor name     :initarg :name)
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



;;; ----------------------------------------------------------------------
;;; Dynamic pages
;;; ----------------------------------------------------------------------

(defclass dynamic-page (page)
  ((request-type :accessor request-type :initarg :request-type)
   (handler      :accessor handler      :initarg :handler) 
   (parameters   :accessor parameters   :initarg :parameters)
   (validators   :accessor validators   :initarg :validators)
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
      (bind-parameters! page (query-string*))
      (let ((output (with-output-to-string (*standard-output*)
                      (apply (body page) (parameters page))))) 
        output)))

(defun build-parameter-list (spec)
  (mapcar (lambda (spec1)
            (destructuring-bind (name &optional
                                      (lisp-type 'string)
                                      validator
                                      requiredp) (ensure-list spec1)
              `(make-instance 'http-parameter
                              :name ',name
                              :lisp-type ',lisp-type
                              :validator ,(or validator '#'identity)
                              :requiredp ,requiredp)))
          spec))

(defun build-parameter-names (spec)
  (mapcar #'first (mapcar #'ensure-list spec)))

(defmacro define-dynamic-page (name (&rest param-spec)
			       (base-url &key 
					 (request-type :get)
					 validators
                                         webapp)
			       &body body)
  `(progn
     (register-page
      (make-instance 'dynamic-page
                     :name ',name
                     :base-url ,base-url 
                     :request-type ,request-type
                     :parameters (list ,@(build-parameter-list param-spec))
                     :validators ,validators
                     :body (lambda (,@(build-parameter-names param-spec)) 
                             ,@body))
      (or ,webapp *webapp*))
     (publish-page ',name)))



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