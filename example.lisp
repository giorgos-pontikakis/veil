(in-package :widgets)

(declaim (optimize (speed 0) (debug 3)))

(defparameter *scrooge*
  (make-instance 'webapp
                 :name 'scrooge 
                 :port 3001
                 :webroot "/scrooge/"
                 :debug-p nil))

(defparameter *foopage*
  (make-instance 'dynamic-page
                 :name 'foo
                 :base-url "actions/foo"
                 :con-type *default-content-type*
                 :request-type :get
                 :parameters (list (make-instance 'http-parameter
                                                  :name :id
                                                  :lisp-type 'integer
                                                  :validator (lambda (int)
                                                               (and (integerp int)
                                                                    (> int 0)))
                                                  :requiredp nil)
                                   (make-instance 'http-parameter
                                                  :name :title
                                                  :lisp-type 'string
                                                  :validator (lambda (string)
                                                               (not (emptyp string)))
                                                  :requiredp nil))
                 :validators (list (list #'(lambda (id title)
                                             (and (= id 1) (string= title "gnp")))
                                         'id 'title))
                 :body (lambda ()
                         (with-html
                           (:h1 "it works!")))))

(register-webapp *scrooge*)
(register-page *foopage* *scrooge*)
(publish-pages *scrooge*)


