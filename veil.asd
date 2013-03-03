;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :cl-user)

(defpackage :veil-asdf
    (:use :cl :asdf))

(in-package :veil-asdf)

(defsystem :veil
  :version "1.0.0"
  :serial t
  ;;
  :depends-on (:alexandria
               :hunchentoot
               :cl-ppcre
               (:version :lisputils "1.0.0"))
  ;;
  :components ((:file "package")
               (:file "utils")
               (:file "acceptors")
               (:file "parameters")
               (:file "pages")
               (:file "paths")))
