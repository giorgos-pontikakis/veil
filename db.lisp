(in-package :veil)



;;; Class definitions

(defclass db ()
  ((dbname  :accessor dbname  :initarg :dbname)
   (dbhost  :accessor dbhost  :initarg :dbhost)
   (dbuser  :accessor dbuser  :initarg :dbuser)
   (dbpass  :accessor dbpass  :initarg :dbpass) 
   (adapter :accessor adapter :initarg :adapter)))

(defparameter *db* nil)

(defmacro define-db (parameter &body body)
  `(progn 
     (defvar ,parameter (make-instance 'db ,@body))
     (setf *db* ,parameter)))


;;; Utilities

(defmacro with-db ((&optional (db *db*)) &body body)
  `(with-connection (list (dbname *db*) (dbuser *db*) (dbpass *db*) (dbhost *db*)) 
     ,@body))

(defmacro select-dao-unique (type &optional (test t) &rest ordering)
  (with-gensyms (dao)
    `(let ((,dao (select-dao ,type ,test ,@ordering)))
       (if (null ,dao)
	   nil
	   (if (= (length ,dao) 1)
	       (first ,dao)
	       (error "DAO is not unique"))))))
