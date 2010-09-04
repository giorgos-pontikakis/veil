(in-package :veil)

(defun group-duplicate-keys (alist &key (test #'eql))
  (iter (with result)
        (for (key . value) in alist)
        (let ((old (assoc key result :test test)))
          (if old
              (rplacd old (list (cdr old) value))
              (push (cons key value) result)))
        (finally (return result))))

(defun parse-query-string (string)
  (mapcar (lambda (name-value)
            (let ((pair (split "=" name-value)))
              (cons (symbolicate (string-upcase (first pair)))
                    (second pair))))
          (split "&" string)))


;;; ----------------------------------------------------------------------
;;; Default CL-WHO configuration
;;; ----------------------------------------------------------------------
(setf *escape-char-p*
      #'(lambda (char)
	  (find char "<>&'\"")))

(setf (html-mode) :xml)

;;; ----------------------------------------------------------------------
;;; HTML macros
;;; ----------------------------------------------------------------------
(defmacro with-html (&body body)
  `(progn
     (with-html-output (*standard-output* nil :prologue nil :indent t)
       ,@body)
     ""))

(defmacro with-page ((&rest html-params) &body body)
  `(progn
     (with-html-output (*standard-output* nil :prologue t :indent nil)
       (:html ,@html-params
	      ,@body))
     nil))

(defmacro html ((&rest args) &body body)
  `(lambda (,@args)
    (with-html
      ,@body)))
