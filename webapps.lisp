(in-package :widgets)

(declaim (optimize (speed 0) (debug 3)))

;;; ----------------------------------------------------------------------
;;; Web applications     
;;; ----------------------------------------------------------------------

(defparameter *webapps* ())

(defclass webapp ()
  ((name           :accessor name           :initarg  :name)
   (root-path      :accessor root-path      :initarg  :root-path)
   (static-path    :accessor static-path    :initarg  :static-path) 
   (pages          :reader   pages          :initform (make-hash-table))
   (port           :accessor port           :initarg  :port)
   (webroot        :accessor webroot        :initarg  :webroot)
   (debug-p        :accessor debug-p        :initarg  :debug-p)
   (acceptor       :accessor acceptor       :initarg  :acceptor)
   (dispatch-table :reader   dispatch-table :initform (make-hash-table))))


(defun find-webapp (name)
  "Take the name (a symbol) and return the webapp object"
  (find name *webapps* :key #'name))

(defun register-webapp (webapp)
  "Push an new webapp into *webapps*, discarding old webapps with the same name"
  (unregister-webapp webapp)
  (push webapp *webapps*))

(defun unregister-webapp (webapp-specifier)
  "Remove a webapp from *webapps*. Specifier must be
  the name of the webapp or the object itself. "
  (setf *webapps*
        (remove (ensure-webapp webapp-specifier) *webapps* :key #'name)))


(defun package-webapp ()
  "Return the webapp with name being the keyword of the current package name."
  (find *webapps*
        (make-keyword (package-name *package*)) :key #'name))

(defun ensure-webapp (webapp-specifier)
  (if (symbolp webapp-specifier)
      (find-webapp webapp-specifier)
      webapp-specifier))

(defun start-webapp (&optional webapp-specifier ssl-p) 
  (let* ((webapp (ensure-webapp webapp-specifier))
         (acceptor (if ssl-p
                       (make-instance 'ssl-acceptor
                                      :port (port webapp)
                                      :request-dispatcher (make-hashtable-request-dispatcher webapp)
                                      :name (name webapp))
                       (make-instance 'acceptor
                                      :port (port webapp) 
                                      :request-dispatcher (make-hashtable-request-dispatcher webapp)
                                      :name (name webapp)))))
    (setf (acceptor webapp) acceptor)
    (start acceptor)))

(defun stop-webapp (&optional (name (package-webapp)))
  (let ((webapp (find-webapp name)))
    (stop (acceptor webapp))))



;; Note: The default Hunchentoot list-request-dispatcher contains a
;; list of functions with no indication of the page that they
;; originated from. The only way to unpublish a page is to (1) delete
;; the dispatch-table list, unregister the function, republish the
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
          (finally (setf (return-code* *reply*) +http-not-found+)))))

