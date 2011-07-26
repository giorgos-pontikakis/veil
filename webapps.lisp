(in-package :veil)



;;; ----------------------------------------------------------------------
;;; Default Hunchentoot configuration
;;; ----------------------------------------------------------------------

(setf *hunchentoot-default-external-format* (flexi-streams:make-external-format :utf-8))
(setf *default-content-type* "text/html; charset=UTF-8")
(setf *use-user-agent-for-sessions* t)
(setf *use-remote-addr-for-sessions* t)
(setf *show-lisp-errors-p* t)
(setf *log-lisp-errors-p* t)
(setf *log-lisp-warnings-p* t)



;;; ----------------------------------------------------------------------
;;; Web Application class
;;; ----------------------------------------------------------------------

(defclass webapp ()
  ((appname        :accessor appname        :initarg  :appname)
   (pkg            :accessor pkg            :initarg  :pkg)
   (database       :accessor database       :initarg  :database)
   (pages          :reader   pages          :initform (make-hash-table))
   (port           :accessor port           :initarg  :port)
   (doc-root       :accessor doc-root       :initarg  :doc-root)
   (fs-root        :accessor fs-root        :initarg  :fs-root)
   (fs-paths       :accessor fs-paths       :initarg  :fs-paths)
   (web-root       :accessor web-root       :initarg  :web-root)
   (web-paths      :accessor web-paths      :initarg  :web-paths)
   (debug-p        :accessor debug-p        :initarg  :debug-p)
   (acceptor-obj   :accessor acceptor-obj   :initarg  :acceptor-obj)
   (use-ssl-p      :reader   use-ssl-p      :initarg  :use-ssl-p)
   (dispatch-table :reader   dispatch-table :initform (make-hash-table))
   (published-p    :accessor published-p    :initarg  :published-p))
  (:default-initargs :use-ssl-p nil :fs-paths '() :web-paths '()))

(defmethod initialize-instance :after ((webapp webapp) &key)
  (setf (acceptor-obj webapp)
        (if (use-ssl-p webapp)
            (make-instance 'ssl-acceptor
                           :port (port webapp)
                           :request-dispatcher (make-hashtable-request-dispatcher webapp)
                           :name (appname webapp))
            (make-instance 'acceptor
                           :port (port webapp)
                           :request-dispatcher (make-hashtable-request-dispatcher webapp)
                           :name (appname webapp))))
  (setf (published-p webapp) nil)
  (setf (pkg webapp) *package*))

(defparameter *webapps* nil)

(defun find-webapp (appname)
  (find appname *webapps* :key #'appname))

(defun register-webapp (webapp)
  (pushnew webapp *webapps* :key #'appname))

(defmacro define-webapp (parameter (&optional webapp-class) &body body)
  `(progn
     (defvar ,parameter (make-instance (or ',webapp-class 'webapp) ,@body))
     (publish-webapp ,parameter)
     (register-webapp ,parameter)))

(defun ensure-webapp (webapp)
  (cond ((and webapp (typep webapp 'webapp))
         webapp)
        ((and webapp (symbolp webapp) (find-webapp webapp)))
        (t
         (error "Webapp not found"))))

(defun package-webapp ()
  (if-let (app (find-symbol "*WEBAPP*" (package-name *package*)))
    (if (boundp app)
        (find-webapp (appname (symbol-value app)))
        (error "Symbol *WEBAPP* is present in package but it is unbound."))
    (find-webapp (package-name *package*))))



;; ----------------------------------------------------------------------
;; Publish and Unpublish
;; ----------------------------------------------------------------------

(defun publish-webapp (&optional webapp)
  (let ((app (ensure-webapp webapp)))
    (if (not (published-p app))
        (progn
          (start (acceptor-obj app))
          (setf (published-p app) t)
          t)
        (warn "Nothing to do: Webapp has already been published."))))

(defun unpublish-webapp (&optional webapp)
  (let ((app (ensure-webapp webapp)))
    (if (published-p app)
        (progn
          (stop (acceptor-obj app))
          (setf (published-p app) nil)
          t)
        (warn "Nothing to do: Webapp has not been published."))))

;; Note: The default Hunchentoot list-request-dispatcher contains a
;; list of functions with no indication of the page that they
;; originated from. The only way to unpublish a page is to delete the
;; dispatch-table list, unregister the function, republish the
;; remaining functions and finally re-register the page (but without
;; re-publishing it).

;; Therefore, we use a tabular data structure -- a hash table. We use
;; the page's name as the key to remove the page from it. In this case,
;; for every request we must transverse only the values of the hash
;; table.

(defun make-hashtable-request-dispatcher (webapp)
  "Make a variation of the default list-request-dispatcher. It uses a
hash table as the dispatch table of the webapp object, instead of
the *dispatch-table* list."
  (lambda (request)
    (let ((*package* (pkg webapp)))
      (iter (for (nil dispatcher) in-hashtable (dispatch-table webapp))
            (for action = (funcall dispatcher request))
            (when action
              (return (funcall action)))
            (finally (setf (return-code* *reply*)
                           +http-not-found+))))))
