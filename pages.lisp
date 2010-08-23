(in-package :widgets)

(declaim (optimize (speed 0) (debug 3)))

;;; ----------------------------------------------------------------------
;;; Web pages
;;; ----------------------------------------------------------------------

(defclass page ()
  ((name     :accessor name     :initarg :name)
   (webapp   :reader   webapp) 
   (base-url :accessor base-url :initarg :base-url)))


(defun find-page (name &optional (webapp-specifier (package-webapp)))
  "Take the page name (a symbol) and a webapp specifier (symbol or
object). Return the page object. "
  (gethash name (pages (ensure-webapp webapp-specifier))))

(defun register-page (page &optional (webapp-specifier (package-webapp)))
  (let ((webapp (ensure-webapp webapp-specifier)))
    (setf (slot-value page 'webapp) webapp)
    (setf (gethash (name page) (pages webapp))
          page)))

(defun unregister-page (page-specifier)
  (let ((page (ensure-page page-specifier)))
    (remhash (name page) (pages (webapp page)))))

(defun full-url (page-specifier)
  (let ((page (ensure-page page-specifier)))
    (concatenate 'string (webroot (webapp page)) (base-url page))))

(defun ensure-page (page-specifier)
  (if (symbolp page-specifier)
      (find-page page-specifier)
      page-specifier))



;;; ----------------------------------------------------------------------
;;; Dynamic pages
;;; ----------------------------------------------------------------------

(defclass dynamic-page (page)
  ((con-type     :accessor con-type     :initarg :con-type)
   (request-type :accessor request-type :initarg :request-type)
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
                (if (string-equal (full-url page)
                                  (script-name request))
                    (handler page)
                    nil)))))

(defmethod handler ((page dynamic-page))
  #'(lambda ()
      (setf (content-type*) (con-type page))
      (with-html-output-to-string (*standard-output*)
        (funcall (body page)))))



;;; ----------------------------------------------------------------------
;;; External pages
;;; ----------------------------------------------------------------------

(defclass external-page (page)
  ((request-type :accessor request-type :initarg :request-type)))



;;; ----------------------------------------------------------------------
;;; Static pages
;;; ----------------------------------------------------------------------

(defclass static-page (page)
  ((con-type  :accessor con-type  :initarg :con-type)
   (path      :accessor path      :initarg :path)
   (builder   :accessor builder   :initarg :builder)
   (publisher :accessor publisher :initarg :publisher)))



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

(defun build-page (page-specifier)
  (%build-page (ensure-page page-specifier)))

(defun build-pages (&optional (webapp-specifier (package-webapp)))
  (iter (for page in (pages (ensure-webapp webapp-specifier)))
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

(defun publish-page (page-specifier)
  (%publish-page (ensure-page page-specifier)))

(defun publish-pages (&optional (webapp-specifier (package-webapp))) 
  (iter (for (nil page) in-hashtable (pages (ensure-webapp webapp-specifier)))
	(%publish-page page)
	(collect (name page))))


;; -- Unpublish --

;; Not implemented -- seems unnecessary.



;; -- Published pages --

;; The same applies here