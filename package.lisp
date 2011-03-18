(in-package :cl-user)

(defpackage :veil
  (:use :common-lisp
        :lisputils
        :hunchentoot
        :cl-who
        :cl-ppcre
        :iterate
        :alexandria
        :postmodern
        :simple-date)
  (:export
   :pack
   ;; db
   :db
   ;; :define-db
   :with-db
   :select-dao-unique
   ;; webapp class
   :*webapps*
   :webapp
   :name
   :pkg
   :database
   ;; :root-path
   ;; :public-path
   ;; :root-dir
   ;; :public-dir
   :pages
   :port
   :fs-root
   :web-root
   :fs-paths
   :web-paths
   :debug-p
   :acceptor-obj
   :use-ssl-p
   :dispatch-table
   :published-p
   ;; webapp functions
   :get-fs-path
   :get-web-path
   :define-webapp
   :find-webapp
   :register-webapp
   :publish-webapp
   :unpublish-webapp
   :package-webapp
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
   :url-fn
   :registers
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
   :urlenc->lisp
   :lisp->urlenc
   :lisp-type
   :val
   :raw
   :validp
   :suppliedp
   :requiredp
   :error-type
   :find-parameter
   :validate-parameters
   :val*))
