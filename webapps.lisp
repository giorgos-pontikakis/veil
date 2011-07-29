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
;;; Veil Acceptors
;;; ----------------------------------------------------------------------

(defclass veil-acceptor-mixin ()
  ((db-connection-spec :accessor db-connection-spec :initarg  :db-connection-spec)
   (doc-root           :accessor doc-root           :initarg  :doc-root)
   (fs-root            :accessor fs-root            :initarg  :fs-root)
   (fs-paths           :accessor fs-paths           :initarg  :fs-paths)
   (web-root           :accessor web-root           :initarg  :web-root)
   (web-paths          :accessor web-paths          :initarg  :web-paths)
   (debug-p            :accessor debug-p            :initarg  :debug-p)
   (acceptor           :accessor acceptor           :initarg  :acceptor)
   (pages              :reader   pages              :initform (make-hash-table))
   (pkg                :reader   pkg)
   (use-ssl-p          :reader   use-ssl-p          :initarg  :use-ssl-p)
   (dispatch-table     :reader   dispatch-table     :initform (make-hash-table)))
  (:default-initargs :use-ssl-p nil :fs-paths '() :web-paths '()))

(defclass veil-acceptor (acceptor veil-acceptor-mixin)
  (request-dispatcher :initform (make-hashtable-request-dispatcher))
  (pkg                :initform *package*))

(defclass veil-ssl-acceptor (ssl-acceptor veil-acceptor-mixin)
  ((request-dispatcher :initform (make-hashtable-request-dispatcher))
   (pkg                :initform *package*)))



;; ----------------------------------------------------------------------
;; Dispatcher
;; ----------------------------------------------------------------------

(defun make-hashtable-request-dispatcher ()
  "Make a variation of the default list-request-dispatcher. It uses a
hash table as the dispatch table of the veil-acceptor, instead of
the *dispatch-table* list."
  (lambda (request)
    (let ((*package* (pkg *acceptor*)))
      (iter (for (nil dispatcher) in-hashtable (dispatch-table *acceptor*))
            (for action = (funcall dispatcher request))
            (when action
              (return (funcall action)))
            (finally (setf (return-code* *reply*)
                           +http-not-found+))))))



;; (defmethod initialize-instance :after ((webapp webapp) &key)
;;   (setf (acceptor webapp)
;;         (if (use-ssl-p webapp)
;;             (make-instance 'ssl-acceptor
;;                            :port (port webapp)
;;                            :request-dispatcher (make-hashtable-request-dispatcher webapp)
;;                            :name (webapp-name webapp)
;;                            :webapp webapp)
;;             (make-instance 'acceptor
;;                            :port (port webapp)
;;                            :request-dispatcher (make-hashtable-request-dispatcher webapp)
;;                            :name (webapp-name webapp)
;;                            :webapp webapp)))
;;   (setf (slot-value webapp 'pkg) *package*))

;; (defparameter *webapps* nil)

;; (defun find-webapp (webapp-name)
;;   (find webapp-name *webapps* :key #'webapp-name))

;; (defun register-webapp (webapp)
;;   (pushnew webapp *webapps* :key #'webapp-name))

;; (defmacro define-webapp (parameter (&optional webapp-class) &body body)
;;   `(progn
;;      (defvar ,parameter (make-instance (or ',webapp-class 'webapp) ,@body))
;;      (publish-webapp ,parameter)
;;      (register-webapp ,parameter)))

;; (defun ensure-webapp (webapp)
;;   (cond ((and webapp (typep webapp 'webapp))
;;          webapp)
;;         ((and webapp (symbolp webapp))
;;          (find-webapp webapp))
;;         (t
;;          (error "Webapp not found"))))

;; (defun package-webapp ()
;;   (if-let (app (find-symbol "*WEBAPP*" (package-name *package*)))
;;     (if (boundp app)
;;         (find-webapp (webapp-name (symbol-value app)))
;;         (error "Symbol *WEBAPP* is present in package but it is unbound."))
;;     (find-webapp (package-name *package*))))

;; (defun current-webapp ()
;;   ())

;; ----------------------------------------------------------------------
;; Publish and Unpublish
;; ----------------------------------------------------------------------

;; (defun publish-webapp (&optional webapp)
;;   (let ((app (ensure-webapp webapp)))
;;     (if (not (published-p app))
;;         (progn
;;           (start (acceptor app))
;;           (setf (slot-value app 'published-p) t))
;;         (warn "Nothing to do: Webapp has already been published."))))

;; (defun unpublish-webapp (&optional webapp)
;;   (let ((app (ensure-webapp webapp)))
;;     (if (published-p app)
;;         (progn
;;           (stop (acceptor app))
;;           (setf (slot-value app 'published-p) nil))
;;         (warn "Nothing to do: Webapp has not been published."))))

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
