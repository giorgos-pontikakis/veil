(in-package :cl-user)

(defpackage :veil
  (:use "COMMON-LISP"
        "ITERATE"
        "ALEXANDRIA"
        "HUNCHENTOOT"
        "CL-PPCRE"
        "LISPUTILS")
  (:export
   ;; Globals
   :*webapps*
   :*page*
   :*parameters*
   ;; Constants
   :+urlenc-true+
   :+urlenc-false+
   :+urlenc-null+
   ;; webapp class
   :webapp
   :webapp-name
   :pkg
   :db-connection-spec
   :pages
   :port
   :doc-root
   :fs-root
   :fs-paths
   :web-root
   :web-paths
   :debug-p
   :acceptor
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
   ;; page class
   :page-name
   :webapp
   :base-url
   :content-type
   :request-type
   :handler
   :parameter-attributes
   ;; Parameter attributes
   :param-name
   :param-key
   :param-page
   :requiredp
   :lisp-type
   :parse-date

   :builder
   :publisher
   :url-fn
   :register-names
   ;; page functions
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
   ;; url encoding
   :urlenc->lisp
   :lisp->urlenc
   ;; parameters
   :attributes
   :val
   :raw
   :validp
   :error-type
   :suppliedp
   :validate-parameters
   ;; paths
   :url
   :url*
   :path
   :url->path
   :path->url
   :path->url*
   ))
