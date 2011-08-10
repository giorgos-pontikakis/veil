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
  ;; Return alist with keys as string. This simulates hunchentoot's
  ;; get-parameters* result for debugging purposes.
  (mapcar (lambda (name-value)
            (let ((pair (split "=" name-value)))
              (cons (string-downcase (first pair))
                    (second pair))))
          (split "&" string)))
