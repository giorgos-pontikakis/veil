(in-package :veil)

(declaim (optimize (speed 0) (debug 3)))

;;; ----------------------------------------------------------------------
;;; Web applications     
;;; ----------------------------------------------------------------------

(defvar *webapps* ())

(defclass webapp ()
  ((name           :accessor name           :initarg  :name)
   (root-path      :accessor root-path      :initarg  :root-path)
   (static-path    :accessor static-path    :initarg  :static-path) 
   (pages          :reader   pages          :initform (make-hash-table))
   (port           :accessor port           :initarg  :port)
   (webroot        :accessor webroot        :initarg  :webroot)
   (debug-p        :accessor debug-p        :initarg  :debug-p)
   (acceptor       :accessor acceptor       :initarg  :acceptor)
   (ssl-p          :reader   ssl-p          :initarg  :ssl-p) 
   (dispatch-table :reader   dispatch-table :initform (make-hash-table))
   (published-p    :accessor published-p    :initarg  :published-p))
  (:default-initargs :ssl-p nil))

(defmethod initialize-instance :after ((webapp webapp) &key)
  (setf (acceptor webapp)
        (if (ssl-p webapp)
            (make-instance 'hunchentoot:ssl-acceptor
                           :port (port webapp)
                           :request-dispatcher (make-hashtable-request-dispatcher webapp)
                           :name (name webapp))
            (make-instance 'hunchentoot:acceptor
                           :port (port webapp) 
                           :request-dispatcher (make-hashtable-request-dispatcher webapp)
                           :name (name webapp))))
  (setf (published-p webapp) nil))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-webapp ((webapp webapp-designator) &body body)
    `(let ((,webapp (ensure-webapp ,webapp-designator)))
       (if webapp
           (progn
             ,@body)
           (error "Webapp not found in *webapps*.")))))

(defun find-webapp (name)
  "Take the name (a symbol) and return the webapp object"
  (find name *webapps* :key #'name))

(defun register-webapp (webapp)
  "Push an new webapp into *webapps*, discarding old webapps with the same name"
  (let ((registered-webapp (find-webapp (name webapp))))
    (when registered-webapp
      ;;; this is triggered during development (recompilations) 

      ;;; this must be written as (overwrite-webapp webapp registered-webapp)
      ;;; where the above functions copies the new object over the
      ;;; old, registered one, without disturbing the accessor. Now,
      ;;; instead, we stop everything and begin again. But I am not
      ;;; sure, maybe the simple thing to do is unregister and stop
      ;;; worrying. It depends on the use, we'll see.

      ;;; Or maybe a defvar solution would be somehow pertinent, i don't know.
      (unregister-webapp registered-webapp)))
  (push webapp *webapps*)
  webapp)

(defun unregister-webapp (webapp-designator)
  "Remove a webapp from *webapps*. If it is already published, unpublish it first."
  (with-webapp (webapp webapp-designator)
    (when (published-p webapp)
      (unpublish-webapp webapp))
    (setf *webapps* (remove webapp *webapps*))))

(defun package-webapp (&optional (package *package*))
  "Return the webapp with name being the keyword of the current package name."
  (or (find (symbolicate (package-name package))
            *webapps* :key #'name)
      (error "No webapp registered with the name of the current package.")))

(defun ensure-webapp (webapp-designator)
  (cond ((symbolp webapp-designator)
         (find-webapp webapp-designator))
        ((typep webapp-designator 'webapp)
         webapp-designator)
        (t (error "The designator is not a webapp or does not designate a registered webapp."))))

(defparameter *webapp* nil)

;; ----------------------------------------------------------------------
;; Publish and Unpublish
;; ----------------------------------------------------------------------

(defun publish-webapp (&optional (webapp-designator (package-webapp)))
  (with-webapp (webapp webapp-designator)
    (if (not (published-p webapp))
        (progn
          (hunchentoot:start (acceptor webapp))
          (setf (published-p webapp) t)
          t)
        (warn "Webapp has already been published"))))

(defun unpublish-webapp (&optional (webapp-designator (package-webapp)))
  (with-webapp (webapp webapp-designator)
    (if (published-p webapp)
        (progn
          (hunchentoot:stop (acceptor webapp))
          (setf (published-p webapp) nil)
          t)
        (warn "Webapp has not been published."))))

;; Note: The default Hunchentoot list-request-dispatcher contains a
;; list of functions with no indication of the page that they
;; originated from. The only way to unpublish a page is to delete the
;; dispatch-table list, unregister the function, republish the
;; remaining functions and finally re-register the page (but without
;; re-publishing it).

;; Therefore, we use a tabular data structure -- a hash table. We use
;; the pages name as the key to remove the page from it. In this case,
;; for every request we must transverse only the values of the hash
;; table. 

(defun make-hashtable-request-dispatcher (webapp)
  "Make a variation of the default list-request-dispatcher. It uses a
hash table as the dispatch table of the webapp object, instead of
the *dispatch-table* list."
  (lambda (request) 
    (iter (for (nil dispatcher) in-hashtable (dispatch-table webapp))
          (for action = (funcall dispatcher request))
          (when action
            (return (funcall action)))
          (finally (setf (hunchentoot:return-code* hunchentoot:*reply*)
                         hunchentoot:+http-not-found+)))))



(defmacro define-webapp ((&optional (name (symbolicate (package-name *package*))))
                         &rest key-args)
  (with-gensyms (app)
    `(progn
       (let ((,app (make-instance 'webapp :name ',name ,@key-args)))
         (register-webapp ,app)
         (publish-webapp ,app)))))

;; (defmacro with-webapp ((&optional webapp-designator) &body body)
;;   `(let ((*webapp* (ensure-webapp (or ,webapp-designator (package-webapp *package*)))))
;;      ,@body))