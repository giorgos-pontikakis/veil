(in-package :veil)



;;; Class definitions

(defclass db ()
  ((dbname  :accessor dbname  :initarg :dbname)
   (dbhost  :accessor dbhost  :initarg :dbhost)
   (dbuser  :accessor dbuser  :initarg :dbuser)
   (dbpass  :accessor dbpass  :initarg :dbpass)
   (adapter :accessor adapter :initarg :adapter)))

(defmacro define-db (parameter &body body)
  `(progn
     (defvar ,parameter (make-instance 'db ,@body))))


;;; Utilities

(defmacro with-db ((&optional db) &body body)
  (with-gensyms (db)
    `(with-connection (list (dbname (symbol-value (intern "*DB*")))
                            (dbuser (symbol-value (intern "*DB*")))
                            (dbpass (symbol-value (intern "*DB*")))
                            (dbhost (symbol-value (intern "*DB*"))))
       ,@body)))

(defmacro select-dao-unique (type &optional (test t) &rest ordering)
  (with-gensyms (dao)
    `(let ((,dao (select-dao ,type ,test ,@ordering)))
       (if (null ,dao)
           nil
           (if (= (length ,dao) 1)
               (first ,dao)
               (error "DAO is not unique"))))))
