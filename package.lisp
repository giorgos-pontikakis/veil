(in-package :cl-user)

(defpackage :veil
  (:use "COMMON-LISP"
        "ITERATE"
        "ALEXANDRIA"
        "HUNCHENTOOT"
        "CL-PPCRE"
        "LISPUTILS")
  (:export
   ;; webapp class
   :*webapps*
   :webapp
   :pkg
   :database
   :pages
   :port
   :doc-root
   :fs-root
   :fs-paths
   :web-root
   :web-paths
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
   :package-webapp
   ;; page classes
   :page
   :base-url
   :content-type
   :request-type
   :handler
   :parameter-attributes
   :validators
   :builder
   :publisher
   :url-fn
   :register-names
   ;; page functions
   :*page*
   :*parameters*
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
   :+urlenc-true+
   :+urlenc-false+
   :+urlenc-null+
   :urlenc->lisp
   :lisp->urlenc
   :lisp-type
   :attributes
   :val
   :raw
   :validp
   :suppliedp
   :requiredp
   :error-type
   :validate-parameters
   :parse-date
   ;; paths
   :url
   :url*
   :path
   :url->path
   :path->url
   :path->url*
   ))
