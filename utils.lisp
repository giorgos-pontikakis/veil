(in-package :widgets)

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
              (cons (make-keyword (string-upcase (first pair)))
                    (second pair))))
          (split "&" string)))
