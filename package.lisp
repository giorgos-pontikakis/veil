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
   ;; db
   :db
   :*db*
   :define-db
   :with-db
   :select-dao-unique
   ;; webapp class
   :webapp
   :*webapp*
   :name
   :root-path
   :static-path
   :pages
   :port
   :webroot
   :debug-p
   :acceptor
   :use-ssl-p
   :dispatch-table
   :published-p
   ;; webapp functions
   :define-webapp
   :publish-webapp
   :unpublish-webapp
   ;; page classes
   :page
   :*page*
   :key
   :webapp
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
   :full-url
   :find-page
   :register-page
   :unregister-page
   :define-dynamic-page
   :build-page
   :build-pages
   :publish-page
   :publish-pages
   ;; parameters
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
