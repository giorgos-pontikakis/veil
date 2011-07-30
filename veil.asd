;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :cl)

(asdf:defsystem :veil
  :serial t
  :depends-on (:iterate
                :alexandria
                :hunchentoot
                :cl-who
                :cl-ppcre
                :lisputils)
  :components ((:file "package")
               (:file "utils")
               (:file "acceptors")
               (:file "parameters")
               (:file "pages")
               (:file "paths")))
