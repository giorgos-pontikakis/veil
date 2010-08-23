;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :cl)

(asdf:defsystem :widgets
  :serial t
  :depends-on (:lisputils
	       :hunchentoot
	       :cl-who 
	       :cl-ppcre 
	       :iterate
               :alexandria
	       :metabang-bind)
  :components ((:file "package")
               (:file "utils")
               (:file "webapps") 
               (:file "pages")
	       (:file "parameters")
               (:file "db")
               (:file "forms")
               (:file "example")))

