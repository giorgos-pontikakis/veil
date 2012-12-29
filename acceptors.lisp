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
;;; Webapps
;;; ----------------------------------------------------------------------

(defclass webapp ()
  ((webapp-name        :accessor webapp-name        :initarg  :webapp-name)
   (fs-root            :accessor fs-root            :initarg  :fs-root)
   (fs-paths           :accessor fs-paths           :initarg  :fs-paths)
   (web-root           :accessor web-root           :initarg  :web-root)
   (web-paths          :accessor web-paths          :initarg  :web-paths)
   (debug-p            :accessor debug-p            :initarg  :debug-p)
   (packages           :reader   packages           :initarg  :packages)
   (autostart          :reader   autostart          :initarg  :autostart)
   (acceptor           :accessor acceptor           :initarg  :acceptor)
   (pages              :reader   pages              :initform (make-hash-table))
   (dispatch-table     :reader   dispatch-table     :initform '()))
  (:default-initargs :fs-paths '()
                     :web-paths '()
                     :packages (list (package-name *package*))
                     :autostart nil
                     :webapp-name (package-name *package*)))

(defclass veil-acceptor (acceptor)
  ((webapps :accessor webapps :initarg :webapps))
  (:default-initargs :webapps '()))

(defclass veil-ssl-acceptor (ssl-acceptor)
  ((webapps :accessor webapps :initarg :webapps))
  (:default-initargs :webapps '()))



;; ----------------------------------------------------------------------
;; Association-list dispatcher
;; ----------------------------------------------------------------------

;; Note: The default Hunchentoot list-request-dispatcher contains a
;; list of functions with no indication of the page that they
;; originated from. The only way to unpublish a page is to delete the
;; dispatch-table list, unregister the function, republish the
;; remaining functions and finally re-register the page (but without
;; re-publishing it).

;; Therefore, we use a tabular data structure -- an alist. We use the
;; page's name as the key to remove the page from it. In this case,
;; for every request we must transverse only the values of the hash
;; table.

(defmethod acceptor-dispatch-request ((acceptor veil-acceptor) request)
  "Make a variation of the default list-request-dispatcher. It uses a
hash table as the dispatch table of the veil-acceptor, instead of
the *dispatch-table* list."
  (loop named app-loop
        for app in (webapps *acceptor*)
        do (loop for (nil . dispatcher) in (dispatch-table app)
                 for action = (funcall dispatcher request)
                 when action
                 do (return-from app-loop (funcall action)))
        finally (setf (return-code* *reply*)
                      +http-not-found+)))



;; ----------------------------------------------------------------------
;; Webapp functions
;; ----------------------------------------------------------------------

(defparameter *webapps* nil)

(defparameter *webapp* nil) ;; The webapp of the page that *acceptor* dispatches

(defun find-webapp (webapp-name)
  (find webapp-name *webapps* :key #'webapp-name :test #'equal))

(defun register-webapp (webapp)
  (pushnew webapp *webapps* :key #'webapp-name)
  (push webapp (webapps (acceptor webapp))))

(defun start-webapp (webapp)
  (start (acceptor webapp)))

(defun stop-webapp (webapp)
  (stop (acceptor webapp)))


(defmacro define-webapp (parameter (&optional webapp-class) &body body)
  (with-gensyms (app)
    `(defvar ,parameter
       (let ((,app (make-instance (or ',webapp-class 'webapp) ,@body)))
         (register-webapp ,app)
         (when (autostart ,app)
           (start-webapp ,app))
         ,app))))

(defun package-webapp ()
  (find-if (lambda (app)
             (member (package-name *package*)
                     (packages app)
                     :test #'string-equal))
           *webapps*))

(defun default-webapp ()
  (or *webapp*
      (package-webapp)))
