(in-package :veil)

(defun group-duplicate-keys (alist &key (test #'eql))
  (loop with result
        for (key . value) in alist
        for old = (assoc key result :test test)
        do (if old
               (rplacd old (list (cdr old) value))
               (push (cons key value) result))
        finally (return result)))

(defun parse-query-string (string)
  ;; Return alist with keys as string. This simulates hunchentoot's
  ;; get-parameters* result for debugging purposes.
  (mapcar (lambda (name-value)
            (let ((pair (split "=" name-value)))
              (cons (string-downcase (first pair))
                    (second pair))))
          (split "&" string)))

(defun princ-http-query (page parameters &optional (stream *standard-output*))
  (loop with delimiter = #\?
        for name in (parameter-names page)
        for val in parameters
        when val
          do (princ delimiter stream)
             (when (char= delimiter #\?)
               (setf delimiter #\&))
             (princ name stream)
             (princ #\= stream)
             (princ (lisp->urlenc val) stream)))
