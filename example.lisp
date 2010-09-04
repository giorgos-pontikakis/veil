(in-package :veil)

(declaim (optimize (speed 0) (debug 3)))

(defvar *scrooge*
  (make-instance 'webapp-db
                 :name 'scrooge 
                 :port 3001
                 :webroot "/scrooge/"
                 :debug-p nil
                 :dbname "scrooge"
                 :dbhost "localhost"
                 :dbuser "gnp"
                 :dbpass ""
                 :adapter "postgres"))

(defparameter *foopage*
  (make-instance 'dynamic-page
                 :name 'foo
                 :base-url "actions/foo"
                 :content-type hunchentoot:*default-content-type*
                 :request-type :get
                 :parameters (list (make-instance 'http-parameter
                                                  :name 'id
                                                  :lisp-type 'integer
                                                  :validator (lambda (int)
                                                               (and (integerp int)
                                                                    (> int 0)))
                                                  :requiredp nil)
                                   (make-instance 'http-parameter
                                                  :name 'title
                                                  :lisp-type 'string
                                                  :validator (lambda (string)
                                                               (not (emptyp string)))
                                                  :requiredp nil))
                 :validators nil #|(list (list #'(lambda (id title)
                                             (and (= id 1) (string= title "gnp")))
                                         'id 'title))|#
                 :body (lambda (id title) 
                         (with-html
                           (:h1 "It works!")
                           (:p (str id))
                           (:p (str title))))))

(define-dynamic-page gnp (foo (bar integer #'plusp)) ("foo" :webapp *scrooge*)
  (with-html 
    (:p "It works as well - foo = "
        (str (or (val foo) "[not given]"))
        " and bar = "
        (str (or (val bar) "[not given]")))))


(register-page *foopage* *scrooge*)
(publish-pages *scrooge*)


