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
   (pages              :reader   pages              :initform (make-hash-table))
   (packages           :reader   packages           :initarg  :packages)
   (dispatch-table     :reader   dispatch-table     :initform '()))
  (:default-initargs :fs-paths '() :web-paths '() :packages (list (package-name *package*))))

(defclass veil-acceptor (acceptor veil-acceptor-mixin)
  ()
  (:default-initargs :name (package-name *package*)
                     :request-dispatcher #'alist-request-dispatcher))

(defclass veil-ssl-acceptor (ssl-acceptor veil-acceptor-mixin)
  ()
  (:default-initargs :name (package-name *package*)
                     :request-dispatcher #'alist-request-dispatcher))



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

(defun alist-request-dispatcher (request)
  "Make a variation of the default list-request-dispatcher. It uses a
hash table as the dispatch table of the veil-acceptor, instead of
the *dispatch-table* list."
  (iter (for (nil . dispatcher) in (dispatch-table *acceptor*))
        (for action = (funcall dispatcher request))
        (when action
          (return (funcall action)))
        (finally (setf (return-code* *reply*)
                       +http-not-found+))))



;; ----------------------------------------------------------------------
;; Acceptor functions
;; ----------------------------------------------------------------------

(defparameter *acceptors* nil)

(defun find-acceptor (acceptor-name)
  (find acceptor-name *acceptors* :key #'acceptor-name :test #'equal))

(defun register-acceptor (acceptor)
  (pushnew acceptor *acceptors* :key #'acceptor-name))

(defmacro define-acceptor (parameter (&optional acceptor-class) &body body)
  `(progn
     (defvar ,parameter (make-instance (or ',acceptor-class 'veil-acceptor) ,@body))
     (register-acceptor ,parameter)
     (start ,parameter)))

(defun default-acceptor ()
  (find-if (lambda (acc)
             (member (package-name *package*)
                     (packages acc)
                     :test #'string-equal))
           *acceptors*))

(defun current-acceptor ()
  (if (boundp '*acceptor*)
      *acceptor*
      (default-acceptor)))
