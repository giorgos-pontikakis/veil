(in-package :veil)



;;; Class definitions

(defclass database ()
  ((dbname  :accessor dbname  :initarg :dbname)
   (dbhost  :accessor dbhost  :initarg :dbhost)
   (dbuser  :accessor dbuser  :initarg :dbuser)
   (dbpass  :accessor dbpass  :initarg :dbpass)
   (adapter :accessor adapter :initarg :adapter)))

;; (defmacro define-db (parameter &body body)
;;   `(progn
;;      (defparameter ,parameter (make-instance 'db ,@body))))


;;; Utilities

(defmacro with-db ((&optional database) &body body)
  (with-gensyms (db)
    `(let ((,db (or ,database (database (package-webapp)))))
       (with-connection (list (dbname (or ,database ,db))
                              (dbuser (or ,database ,db))
                              (dbpass (or ,database ,db))
                              (dbhost (or ,database ,db)))
         ,@body))))

(defmacro select-dao-unique (type &optional (test t) &rest ordering)
  (with-gensyms (dao)
    `(let ((,dao (select-dao ,type ,test ,@ordering)))
       (if (null ,dao)
           nil
           (if (= (length ,dao) 1)
               (first ,dao)
               (error "DAO is not unique"))))))
