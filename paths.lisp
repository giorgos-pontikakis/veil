(in-package :veil)

;;; auxiliary

(defun princ-url (arg)
  (unless (or (null arg)
              (eql arg :null)
              (and (stringp arg)
                   (emptyp arg)))
    (if-let (web-path (get-web-path arg))
      (princ web-path)
      (princ arg))))

(defun get-fs-path (id)
  "Given an identifier, which is a symbol, return the filesystem path"
  (cdr (assoc id (fs-paths (current-acceptor)))))

(defun get-web-path (id)
  "Given an identifier, which is a symbol, return the web path"
  (cdr (assoc id (web-paths (current-acceptor)))))



;;; exported stuff

(defun url* (&rest args)
  "Concatenates its arguments to produce a url path. Expects string or
symbols as arguments. Symbols are used as keys to get values from
web-paths of current-acceptor."
  (unless (null args)
    (with-output-to-string (*standard-output*)
      (mapc #'princ-url args))))

(defun url (&rest args)
  "Same as url*, but it prepends web-root of current-acceptor"
  (with-output-to-string (*standard-output*)
    (princ (web-root (current-acceptor)))
    (unless (null args)
      (mapc #'princ-url args))))

(defun path (&rest args)
  "Concatenates its arguments, prepending fs-root, to produce an
  absolute pathname. Expects string or symbols as arguments. Symbols
  are used as keys to get values from fs-paths of current-acceptor. If
  the final element is a string not ending with a slash, it is treated
  as filename "
  (let ((path (mapcar (lambda (arg)
                        (cond ((symbolp arg)
                               (or (get-fs-path arg) #p""))
                              ((stringp arg)
                               (make-pathname :directory `(:relative ,@(split "/" arg))))
                              ((pathnamep arg)
                               arg)
                              (t (error "Invalid input."))))
                      args)))
    ;; If the final element does not end with a slash, convert to a
    ;; file pathname and replace it in the path list destructively
    (let ((final (car (last args))))
      (when (and (stringp final)
                 (not (emptyp final))
                 (not (ends-with #\/ final)))
        (rplaca (last path) (cl-fad:pathname-as-file (car (last path))))))
    (reduce (lambda (path1 path2)
              (merge-pathnames path1 path2))
            (nreverse (cons (fs-root (current-acceptor)) path)))))


(defun url->path (url)
  (path (subseq url (length (web-root (current-acceptor))))))

(defun path->url (pathname &optional root)
  (url (enough-namestring pathname
                          (or root
                              (fs-root (current-acceptor))))))

(defun path->url* (pathname &optional root)
  (url* (enough-namestring pathname
                           (or root
                               (fs-root (current-acceptor))))))