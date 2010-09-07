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
  ;; Return alist with keys as keywords not symbols, because this is
  ;; run by hunchentoot in a separate thread in the common-lisp-user
  ;; package, so the symbols differ from whatever package we are in.
  (mapcar (lambda (name-value)
            (let ((pair (split "=" name-value)))
              (cons (make-keyword (string-upcase (first pair)))
                    (second pair))))
          (split "&" string)))


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
