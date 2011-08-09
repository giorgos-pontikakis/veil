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
   :doc-root
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
   :default-acceptor
   :current-acceptor
   ;; page class
   :page-name
   :base-url
   :content-type
   :request-type
   :acceptor
   :body
   :parameter-attributes
   ;; Parameter attributes
   :parameter-name
   :parameter-key
   :parameter-page
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
   :build-page
   :build-pages
   :publish-page
   :publish-pages
   :unpublish-page
   :published-pages
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
   ))
