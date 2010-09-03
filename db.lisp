(in-package :widgets)

(defclass db-mixin ()
  ((dbname  :accessor dbname  :initarg :dbname)
   (dbhost  :accessor dbhost  :initarg :dbhost)
   (dbuser  :accessor dbuser  :initarg :dbuser)
   (dbpass  :accessor dbpass  :initarg :dbpass) 
   (adapter :accessor adapter :initarg :adapter)))

(defclass webapp-db (webapp db)
  ())

(defmacro with-db (&body body)
  `(with-webapp ()
     (with-connection (list (dbname*webapp*) (dbuser *webapp*) (dbpass *webapp*) (dbhost *webapp*)) 
       ,@body)))

(defmacro select-dao-unique (type &optional (test t) &rest ordering)
  (with-gensyms (dao)
    `(let ((,dao (select-dao ,type ,test ,@ordering)))
       (if (null ,dao)
	   nil
	   (if (= (length ,dao) 1)
	       (first ,dao)
	       (error "DAO is not unique"))))))