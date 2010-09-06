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
;;; Default CL-WHO configuration
;;; ----------------------------------------------------------------------
(setf *escape-char-p*
      #'(lambda (char)
	  (find char "<>&'\"")))

(setf (html-mode) :xml)
(setf hunchentoot:*hunchentoot-default-external-format* (flexi-streams:make-external-format :utf-8))
(setf hunchentoot:*default-content-type* "text/html; charset=UTF-8") 
(setf hunchentoot:*use-user-agent-for-sessions* t)
(setf hunchentoot:*use-remote-addr-for-sessions* t)
(setf hunchentoot:*show-lisp-errors-p* t)
(setf hunchentoot:*log-lisp-errors-p* t)
(setf hunchentoot:*log-lisp-warnings-p* t)


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
