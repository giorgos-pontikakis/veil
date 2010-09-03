(in-package :widgets)

(defclass db-mixin ()
  ((host     :accessor host     :initarg :host)
   (username :accessor username :initarg :username)
   (password :accessor password :initarg :password) 
   (adapter  :accessor adapter  :initarg :adapter)
   (dbname   :accessor dbname   :initarg :dbname)))

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