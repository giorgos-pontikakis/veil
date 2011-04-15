(in-package :veil)



;;; ----------------------------------------------------------------------
;;; Web pages
;;; ----------------------------------------------------------------------

(defclass page ()
  ((name         :accessor name         :initarg :name)
   (key          :accessor key          :initarg :key)
   (webapp       :reader   webapp)
   (base-url     :accessor base-url     :initarg :base-url)
   (content-type :accessor content-type :initarg :content-type)
   (body         :accessor body         :initarg :body)
   (request-type :accessor request-type :initarg :request-type)
   (parameters   :accessor parameters   :initarg :parameters)))

(defgeneric publisher (page)
  (:documentation "Return a function which, when called, returns the
  dispatcher for a page"))

(defun find-page (name webapp)
  "Take the page name (a symbol) and a webapp designator (symbol or
object). Return the page object. "
  (gethash name (pages (ensure-webapp webapp))))

(defun register-page (page webapp)
  "Add a page to a webapp's pages"
  (let ((app (ensure-webapp webapp)))
    (setf (slot-value page 'webapp) app)
    (setf (gethash (name page) (pages app))
          page)))

(defun unregister-page (page-name webapp)
  "Remove a page from a webapp's pages"
  (let ((page (find-page page-name webapp)))
    (if page
        (remhash (name page) (pages (webapp page)))
        (error "Page ~A not found." page-name))))

(defparameter *page* nil)

(defmacro define-page-fn (page-name webapp &optional arguments)
  (with-gensyms (page param-value-alist)
    `(defun ,page-name ,(cons '&key (append arguments (list 'fragment)))
       (declare (ignorable fragment))
       (let ((,page (find-page ',page-name ,webapp))
             (,param-value-alist (iter (for arg in ',arguments)
                                       (for val in (list ,@arguments))
                                       (when val
                                         (collect (cons arg val))))))
         (concatenate 'string
                      (web-root (webapp ,page))
                      (base-url ,page)
                      (make-query-string ,param-value-alist))))))

(defmacro define-regex-page-fn (page-name webapp registers &optional arguments)
  (with-gensyms (page param-value-alist)
    `(defun ,page-name ,(append registers
                                (cons '&key (append arguments (list 'fragment))))
       (declare (ignorable fragment))
       (let ((,page (find-page ',page-name ,webapp))
             (,param-value-alist (iter (for arg in ',arguments)
                                       (for val in (list ,@arguments))
                                       (when val
                                         (collect (cons arg val))))))
         (concatenate 'string
                      (web-root (webapp ,page))
                      (funcall (url-fn ,page) ,@registers)
                      (make-query-string ,param-value-alist))))))



;;; ----------------------------------------------------------------------
;;; Local utilities
;;; ----------------------------------------------------------------------

(defun build-parameter-list (page specs)
  (mapcar (lambda (spec)
            (destructuring-bind (name &optional
                                      (lisp-type 'string)
                                      vspec
                                      requiredp) (ensure-list spec)
              `(make-instance 'http-parameter
                              :name ',name
                              :page ,page
                              :key (make-keyword ',name)
                              :lisp-type ',lisp-type
                              :vfn ,(cond ((consp vspec) `(symbol-function ',(first vspec)))
                                          ((null vspec) '(constantly nil))
                                          (t `(symbol-function ',vspec)))
                              :vargs ',(cond ((consp vspec) (rest vspec))
                                             ((null vspec) nil)
                                             (t (list name)))
                              :requiredp ',requiredp)))
          specs))


(defun build-parameter-names (spec)
  (mapcar #'first (mapcar #'ensure-list spec)))

(defun full-base-url (page)
  (concatenate 'string (web-root (webapp page)) (base-url page)))



;;; ----------------------------------------------------------------------
;;; Dynamic and regex pages
;;; ----------------------------------------------------------------------

(defgeneric handler (page &key)
  (:documentation "Return a function which, when called, returns the
  handler for a page of class dynamic-page or its subclasses"))


;;; --- Dynamic pages ---

(defclass dynamic-page (page)
  ())

(defmethod publisher ((page dynamic-page))
  #'(lambda ()
      (setf (gethash (name page)
                     (dispatch-table (webapp page)))
            (lambda (request)
              (if (string= (full-base-url page)
                           (script-name request))
                  (progn
                    (set-parameters page)
                    (handler page))
                  nil)))))

(defmethod handler ((page dynamic-page) &key)
  #'(lambda ()
      (let ((*page* page))
        (declare (special *page*))
        (with-output-to-string (*standard-output*)
          (apply (body page) (parameters page))))))

(defmacro define-dynamic-page (name (base-url &key
                                              (request-type :get)
                                              (content-type *default-content-type*)
                                              webapp-name)
                               (&rest param-specs) &body body)
  (with-gensyms (page parameters webapp)
    (let ((parameter-names (build-parameter-names param-specs)))
      `(let* ((,webapp (find-webapp ',webapp-name))
              (,page (make-instance 'dynamic-page
                                    :name ',name
                                    :key (make-keyword ',name)
                                    :base-url ,base-url
                                    :request-type ,request-type
                                    :content-type ,content-type
                                    :body (lambda (,@parameter-names)
                                            ,@body)))
              (,parameters (list ,@(build-parameter-list page param-specs))))
         (register-page ,page (or ,webapp (package-webapp)))
         (setf (parameters ,page) ,parameters)
         (define-page-fn ,name (or ,webapp (package-webapp)) ,parameter-names)
         (publish-page ',name (or ,webapp (package-webapp)))))))



;;; --- Regex pages ---

(defclass regex-page (dynamic-page)
  ((scanner         :accessor scanner         :initarg :scanner)
   (register-names  :accessor register-names  :initarg :register-names)
   (register-groups :accessor register-groups :initarg :register-groups)
   (url-fn          :accessor url-fn          :initarg :url-fn)))

(defmethod publisher ((page regex-page))
  (lambda ()
    (setf (gethash (name page)
                   (dispatch-table (webapp page)))
          (lambda (request)
            (multiple-value-bind (match regs) (scan-to-strings (scanner page)
                                                               (script-name request))
              (if match
                  (progn
                    (set-parameters page)
                    (handler page :register-values (coerce regs 'list)))
                  nil))))))

(defmethod handler ((page regex-page) &key register-values)
  (lambda ()
    (let ((*page* page))
      (declare (special *page*))
      (with-output-to-string (*standard-output*)
        (apply (body page)
               (append (parameters page) register-values))))))

(defmacro define-regex-page (name (base-url &key
                                            (request-type :get)
                                            (content-type *default-content-type*)
                                            webapp-name)
                             (&rest param-specs) &body body)
  (with-gensyms (page parameters webapp)
    (let ((param-names (build-parameter-names param-specs))
          (register-names (mapcan (lambda (item)
                                    (if (listp item)
                                        (list (first item))
                                        nil))
                                  base-url))
          (register-groups (mapcan (lambda (item)
                                     (if (listp item)
                                         (list (concatenate  'string "(" (second item) ")"))
                                         nil))
                                   base-url)))
      `(let* ((,webapp (find-webapp ',webapp-name))
              (,page (make-instance 'regex-page
                                    :name ',name
                                    :key (make-keyword ',name)
                                    :base-url ',base-url
                                    :register-names ',register-names
                                    :register-groups ',register-groups
                                    :url-fn (lambda ,register-names
                                              (concatenate 'string ,@(mapcar (lambda (item)
                                                                               (if (listp item)
                                                                                   (first item)
                                                                                   item))
                                                                             base-url)))
                                    :request-type ,request-type
                                    :content-type ,content-type
                                    :body (lambda (,@param-names ,@register-names)
                                            ,@body)))
              (,parameters (list ,@(build-parameter-list page param-specs))))
         (register-page ,page (or ,webapp (package-webapp)))
         (setf (parameters ,page) ,parameters)
         (setf (scanner ,page)
               (create-scanner (concatenate 'string
                                            "^"
                                            (web-root (webapp ,page))
                                            ,@register-groups
                                            "$")))
         (define-regex-page-fn ,name (or ,webapp (package-webapp)) ,register-names ,param-names)
         (publish-page ',name (or ,webapp (package-webapp)))))))



;;; ----------------------------------------------------------------------
;;; Static pages
;;; ----------------------------------------------------------------------

(defclass static-page (page)
  ((location :accessor location :initarg :location)))

(defmethod publisher ((page static-page))
  (lambda ()
    (setf (gethash (name page)
                   (dispatch-table (webapp page)))
          (lambda (request)
            (if (string= (full-base-url page)
                         (script-name request))
                (handle-static-file (location page) (content-type page))
                nil)))))

(defgeneric builder (page)
  (:documentation "Return a function which, when called, writes the
  body of the page to the path of the page"))

(defmethod builder ((page static-page))
  (lambda ()
    (ensure-directories-exist (location page))
    (with-open-file (stream (location page)
                            :element-type 'base-char
                            :direction :output
                            :if-does-not-exist :create
                            :if-exists :supersede)
      (let ((*standard-output* stream))
        (funcall (body page))))))

(defmacro define-static-page (name (base-url &key
                                             (request-type :get)
                                             (content-type *default-content-type*)
                                             webapp-name
                                             location)
                              (&rest param-specs)
                              &body body)
  (with-gensyms (page parameters webapp)
    (let ((parameter-names (build-parameter-names param-specs)))
      `(let* ((,webapp (find-webapp ,webapp-name))
              (,page (make-instance 'static-page
                                    :name ',name
                                    :key (make-keyword ',name)
                                    :base-url ,base-url
                                    :location ,location
                                    :request-type ,request-type
                                    :content-type ,content-type
                                    :body (lambda ()
                                            ,@body)))
              (,parameters (list ,@(build-parameter-list page param-specs))))
         (setf (parameters ,page) ,parameters)
         (register-page ,page (or ,webapp (package-webapp)))
         (unless (location ,page)
           (setf (location ,page) (static-page-pathname ,page)))
         (define-page-fn ,name (or ,webapp (package-webapp)) ,parameter-names)
         (publish-page ',name (or ,webapp (package-webapp)))))))

(defun static-page-pathname (page)
  (let ((relative-path
         (if (ends-with #\/ (base-url page))
             (make-pathname :directory (if (emptyp (base-url page))
                                           `(:relative ,@(split "/" (base-url page)))
                                           nil)
                            :name "index"
                            :type "html")
             (cl-fad:pathname-as-file
              (make-pathname :directory `(:relative ,@(split "/" (base-url page))))))))
    (merge-pathnames relative-path (doc-root (package-webapp)))))



;; ----------------------------------------------------------------------
;; Build, Publish and Unpublish
;; ----------------------------------------------------------------------

;; -- Build --

(defun build-page (page)
  (funcall (builder page))
  (values))

(defun build-pages (webapp)
  (let ((app (ensure-webapp webapp)))
    (iter (for (nil page) in-hashtable (pages app))
          (when (eql (type-of page) 'static-page)
            (funcall (builder page))
            (collect (name page))))))



;; -- Publish --

(defun publish-page (page-name webapp)
  (funcall (publisher (find-page page-name webapp))))

(defun publish-pages (webapp)
  (iter (for (nil page) in-hashtable (pages webapp))
        (unless (eql (type-of page) 'static-page)
          (funcall (publisher page))
          (collect (name page)))))


;; -- Unpublish --

;; Not implemented yet.



;; -- Published pages --

(defun published-pages (webapp)
  (iter (for (name nil) in-hashtable (dispatch-table (ensure-webapp webapp)))
        (collect name)))