(in-package :cl-user)

(defpackage :veil
  (:use :common-lisp
        :lisputils
        :hunchentoot
        :cl-who
        :cl-ppcre
        :iterate
        :alexandria
        :metabang-bind
        :postmodern)
  (:export
   :pack
   ;; db
   :db
   :define-db
   :with-db
   :select-dao-unique
   ;; webapp class
   :*webapps*
   :webapp
   :name
   :root-path
   :static-path
   :pages
   :port
   :webroot
   :debug-p
   :acceptor-obj
   :use-ssl-p
   :dispatch-table
   :published-p
   ;; webapp functions
   :define-webapp
   :find-webapp
   :register-webapp
   :publish-webapp
   :unpublish-webapp
   ;; page classes
   :page
   :key
   :base-url
   :content-type
   :request-type
   :handler
   :parameters
   :validators
   :body
   :path
   :builder
   :publisher
   ;; page functions
   :*page*
   :dynamic-page
   :static-page
   :external-page
   :find-page
   :register-page
   :unregister-page
   :define-dynamic-page
   :define-regex-page
   :define-static-page
   :with-registers
   :build-page
   :build-pages
   :publish-page
   :publish-pages
   ;; parameters
   :+html-true+
   :+html-false+
   :+html-null+
   :lisp->html
   :html->lisp
   :lisp-type
   :val
   :raw
   :validp
   :suppliedp
   :requiredp
   :error-type
   :find-parameter
   :val*
   ;; html
   :indent
   :with-html
   :defhtml
   :html
   :with-document
   :render
   :url))
