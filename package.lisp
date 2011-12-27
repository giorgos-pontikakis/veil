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
   :*acceptors*
   :*page*
   :*parameters*
   ;; Constants
   :+urlenc-true+
   :+urlenc-false+
   :+urlenc-null+
   ;; acceptor class
   :veil-acceptor
   :packages
   :db-connection-spec
   :pages
   :fs-root
   :fs-paths
   :web-root
   :web-paths
   :debug-p
   :acceptor
   :dispatch-table
   :published-p
   ;; acceptor functions
   :define-acceptor
   :find-acceptor
   :register-acceptor
   :package-acceptor
   :default-acceptor
   ;; page class
   :page-name
   :base-url
   :content-type
   :request-type
   :acceptor
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
