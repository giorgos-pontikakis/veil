(in-package :veil)

(declaim (optimize (speed 0) (debug 3)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-html (&body body)
    ;; We return nil so that we can use this inside the render
    ;; function without writing the return value of with-html-output,
    ;; which is garbage, to the output string
    `(with-html-output (*standard-output* nil :prologue nil :indent nil)
       ,@body
       nil)))

(defmacro defhtml (name args &body body)
  `(defun ,name (&key ,@args)
     (lambda (&key ,@(mapcar #'list args args))
       (with-html
         ,@body))))

(defmacro html ((&rest args) &body body)
  `(lambda (,@args)
     (with-html
       ,@body)))

(defmacro with-document ((&optional spec &rest html-params) &body body)
  (ecase spec
    ((:xhtml nil)
     `(progn
        (setf (html-mode) :xml)
        (with-html-output (*standard-output* nil :prologue t :indent t)
          (:html ,@html-params
            ,@body))))
    ((:html4)
     `(progn
        (setf (html-mode) :sgml)
        (with-html-output (*standard-output* nil :prologue t :indent t)
          (:html ,@html-params
            ,@body))))
    ((:xml)
     `(progn
        (setf (html-mode) :xml)
        (with-html-output (*standard-output* nil :prologue nil :indent t)
          (fmt "<?xml version=\"1.0\" encoding=\"utf-8\"?>~&")
          (:html ,@html-params
            ,@body))))))

(defun render (html &rest args)
  (cond ((null html) "")
        ((functionp html)
         (apply html args))
        ((listp html)
         (mapc #'render html))
        (t (with-html
             (str (lisp->html html))))))

(defun url (&rest args)
  "Take a list of arguments, convert them to html, concatenate and
print them as a url. If the first argument does not seem to be an
absolute url reference, prepend the webroot, otherwise print the
scheme and machine normally. "
  (with-output-to-string (*standard-output*)
    (unless (null args)
      (bind ((#(scheme machine nil script-name)
               (nth-value 1 (scan-to-strings "(https?:|ftp:)?(//[^/]+)?(/?)(.*)" (first args)))))
        (if machine
            (progn (princ (or scheme "http:"))
                   (princ machine)
                   (princ "/")
                   (princ script-name)
                   (mapc #'princ
                         (mapcar #'lisp->html (rest args))))
            (progn (princ (symbol-value (intern "*WEBAPP*" *package*)))
                   (mapc #'princ
                         (mapcar #'lisp->html args))))))))
