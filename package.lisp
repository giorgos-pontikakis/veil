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
   :*webapp*
   :*page*
   :*parameters*
   :*registers*
   ;; Constants
   :+urlenc-true+
   :+urlenc-false+
   :+urlenc-null+
   ;; veil acceptors
   :veil-acceptor
   :veil-ssl-acceptor
   ;; webapp class
   :packages
   :pages
   :fs-root
   :fs-paths
   :web-root
   :web-paths
   :debug-p
   :autostart
   :webapp
   :dispatch-table
   :published-p
   :webapp-name
   ;; webapp functions
   :define-webapp
   :find-webapp
   :register-webapp
   :start-webapp
   :stop-webapp
   :package-webapp
   :default-webapp
   ;; page class
   :page-name
   :base-url
   :content-type
   :request-type
   :webapp
   :parameter-attributes
   :page-url
   ;; Parameter attributes
   :parameter-name
   :parameter-key
   :parameter-page
   :requiredp
   :lisp-type
   :parse-date
   :builder
   :publisher
   :register-names
   ;; page functions
   :page
   :dynamic-page
   :regex-page
   :static-page
   :find-page
   :register-page
   :unregister-page
   :defpage
   :build-page
   :build-pages
   :publish-page
   :publish-pages
   :unpublish-page
   :published-pages
   :define-page-function
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
   :find-parameter
   ;; paths
   :url
   :url*
   :path
   :url->path
   :path->url
   :path->url*
   ;; conditions
   :http-parse-error
   :validation-error
   ))
