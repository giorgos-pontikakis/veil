(in-package :veil)



;;; ----------------------------------------------------------------------
;;; Web pages
;;; ----------------------------------------------------------------------

(defclass page ()
  ((page-name            :accessor page-name            :initarg :page-name)
   (page-key             :accessor page-key             :initarg :page-key)
   (webapp               :reader   webapp)
   (base-url             :accessor base-url             :initarg :base-url)
   (content-type         :accessor content-type         :initarg :content-type)
   (body                 :accessor body                 :initarg :body)
   (request-type         :accessor request-type         :initarg :request-type)
   (parameter-attributes :accessor parameter-attributes :initarg :parameter-attributes)))

(defgeneric publisher (page)
  (:documentation "Return a function which, when called, returns the
  dispatcher for a page"))

(defun find-page (page-name webapp)
  "Take the page name (a symbol) and a webapp designator (symbol or
object). Return the page object. "
  (gethash page-name (pages (ensure-webapp webapp))))

(defun register-page (page webapp)
  "Add a page to a webapp's pages"
  (let ((app (ensure-webapp webapp)))
    (setf (slot-value page 'webapp) app)
    (setf (gethash (page-name page) (pages app))
          page)))

(defun unregister-page (page-name webapp)
  "Remove a page from a webapp's pages"
  (let ((page (find-page page-name webapp)))
    (if page
        (remhash (page-name page) (pages (webapp page)))
        (error "Page ~A not found." page-name))))

(defparameter *page* nil)
(defparameter *parameters* nil)

(defmacro define-page-fn (page-name webapp &optional arguments)
  (with-gensyms (page param-value-alist)
    `(defun ,page-name ,(cons '&key (append arguments (list 'fragment)))
       (declare (ignorable ,@arguments fragment))
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
       (declare (ignorable ,@arguments fragment))
       (let ((,page (find-page ',page-name ,webapp))
             (,param-value-alist (iter (for arg in ',arguments)
                                       (for val in (list ,@arguments))
                                       (when val
                                         (collect (cons arg val))))))
         (concatenate 'string
                      (web-root (webapp ,page))
                      (apply (url-fn ,page) (mapcar #'lisp->urlenc (list ,@registers)))
                      (make-query-string ,param-value-alist))))))



;;; ----------------------------------------------------------------------
;;; Parameter attributes
;;; ----------------------------------------------------------------------

(defclass http-parameter-attributes ()
  ((param-name :accessor param-name :initarg :param-name)
   (param-key  :accessor param-key  :initarg :param-key)
   (param-page :accessor param-page :initarg :param-page)
   (lisp-type  :accessor lisp-type  :initarg :lisp-type)
   (vfn        :accessor vfn        :initarg :vfn)
   (vargs      :accessor vargs      :initarg :vargs)
   (requiredp  :accessor requiredp  :initarg :requiredp)))

(defun build-parameter-attributes (page specs)
  (mapcar (lambda (spec)
            (destructuring-bind (param-name &optional
                                      (lisp-type 'string)
                                      vspec
                                      requiredp) (ensure-list spec)
              `(make-instance 'http-parameter-attributes
                              :param-name ',param-name
                              :param-page ,page
                              :param-key (make-keyword ',param-name)
                              :lisp-type ',lisp-type
                              :vfn ,(cond ((consp vspec) `(symbol-function ',(first vspec)))
                                          ((null vspec) '(constantly nil))
                                          (t `(symbol-function ',vspec)))
                              :vargs ',(cond ((consp vspec) (rest vspec))
                                             ((null vspec) nil)
                                             (t (list param-name)))
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
      (setf (gethash (page-name page)
                     (dispatch-table (webapp page)))
            (lambda (request)
              (if (string= (full-base-url page)
                           (script-name request))
                  (handler page)
                  nil)))))

(defmethod handler ((page dynamic-page) &key)
  #'(lambda ()
      (let ((*page* page)
            (*parameters* (parse-parameters page)))
        (with-output-to-string (*standard-output*)
          (apply (body page) *parameters*)))))

(defmacro define-dynamic-page (page-name (base-url &key (request-type :get)
                                                        (content-type *default-content-type*)
                                                        webapp-name)
                               (&rest param-specs) &body body)
  (with-gensyms (page parameter-attributes webapp)
    (let ((parameter-names (build-parameter-names param-specs)))
      `(let* ((,webapp (find-webapp ',webapp-name))
              (,page (make-instance 'dynamic-page
                                    :page-name ',page-name
                                    :page-key (make-keyword ',page-name)
                                    :base-url ,base-url
                                    :request-type ,request-type
                                    :content-type ,content-type
                                    :body (lambda (,@parameter-names)
                                            (declare (ignorable ,@parameter-names))
                                            ,@body)))
              (,parameter-attributes (list ,@(build-parameter-attributes page param-specs))))
         (register-page ,page (or ,webapp (package-webapp)))
         (setf (parameter-attributes ,page) ,parameter-attributes)
         (define-page-fn ,page-name (or ,webapp (package-webapp)) ,parameter-names)
         (publish-page ',page-name (or ,webapp (package-webapp)))))))



;;; --- Regex pages ---

(defclass regex-page (dynamic-page)
  ((scanner         :accessor scanner         :initarg :scanner)
   (register-names  :accessor register-names  :initarg :register-names)
   (url-fn          :accessor url-fn          :initarg :url-fn)))

(defmethod publisher ((page regex-page))
  (lambda ()
    (setf (gethash (page-name page)
                   (dispatch-table (webapp page)))
          (lambda (request)
            (multiple-value-bind (match regs) (scan-to-strings (scanner page)
                                                               (script-name request))
              (if match
                  (handler page :register-values (coerce regs 'list))
                  nil))))))

(defmethod handler ((page regex-page) &key register-values)
  (lambda ()
    (let ((*page* page)
          (*parameters* (parse-parameters page)))
      (with-output-to-string (*standard-output*)
        (apply (body page)
               (append *parameters* register-values))))))

(defmacro define-regex-page (page-name (base-url &key
                                            (request-type :get)
                                            (content-type *default-content-type*)
                                            webapp-name)
                             (&rest param-specs) &body body)
  (with-gensyms (page parameter-attributes webapp)
    (let ((param-names (build-parameter-names param-specs))
          (register-names (mapcan (lambda (item)
                                    (if (listp item)
                                        (list (first item))
                                        nil))
                                  base-url)))
      `(let* ((,webapp (find-webapp ',webapp-name))
              (,page (make-instance 'regex-page
                                    :page-name ',page-name
                                    :page-key (make-keyword ',page-name)
                                    :base-url ',base-url
                                    :register-names ',register-names
                                    :url-fn (lambda ,register-names
                                              (concatenate 'string ,@(mapcar (lambda (item)
                                                                               (if (listp item)
                                                                                   (first item)
                                                                                   item))
                                                                             base-url)))
                                    :request-type ,request-type
                                    :content-type ,content-type
                                    :body (lambda (,@param-names ,@register-names)
                                            (declare (ignorable ,@param-names ,@register-names))
                                            ,@body)))
              (,parameter-attributes (list ,@(build-parameter-attributes page param-specs))))
         (register-page ,page (or ,webapp (package-webapp)))
         (setf (parameter-attributes ,page) ,parameter-attributes)
         (setf (scanner ,page)
               (create-scanner (concatenate 'string
                                            "^"
                                            (web-root (webapp ,page))
                                            ,@(mapcar (lambda (item)
                                                        (if (listp item)
                                                            (second item)
                                                            item))
                                                      base-url)
                                            "$")))
         (define-regex-page-fn ,page-name (or ,webapp (package-webapp)) ,register-names ,param-names)
         (publish-page ',page-name (or ,webapp (package-webapp)))))))



;;; ----------------------------------------------------------------------
;;; Static pages
;;; ----------------------------------------------------------------------

(defclass static-page (page)
  ((location :accessor location :initarg :location)))

(defmethod publisher ((page static-page))
  (lambda ()
    (setf (gethash (page-name page)
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

(defmacro define-static-page (page-name (base-url &key (request-type :get)
                                                  (content-type *default-content-type*)
                                                  webapp-name
                                                  location)
                              (&rest param-specs)
                              &body body)
  (with-gensyms (page parameter-attributes webapp)
    (let ((parameter-names (build-parameter-names param-specs)))
      `(let* ((,webapp (find-webapp ,webapp-name))
              (,page (make-instance 'static-page
                                    :page-name ',page-name
                                    :page-key (make-keyword ',page-name)
                                    :base-url ,base-url
                                    :location ,location
                                    :request-type ,request-type
                                    :content-type ,content-type
                                    :body (lambda ()
                                            ,@body)))
              (,parameter-attributes (list ,@(build-parameter-attributes page param-specs))))
         (setf (parameter-attributes ,page) ,parameter-attributes)
         (register-page ,page (or ,webapp (package-webapp)))
         (unless (location ,page)
           (setf (location ,page) (static-page-pathname ,page)))
         (define-page-fn ,page-name (or ,webapp (package-webapp)) ,parameter-names)
         (publish-page ',page-name (or ,webapp (package-webapp)))))))

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
            (collect (page-name page))))))



;; -- Publish --

(defun publish-page (page-name webapp)
  (funcall (publisher (find-page page-name webapp))))

(defun publish-pages (webapp)
  (iter (for (nil page) in-hashtable (pages webapp))
        (unless (eql (type-of page) 'static-page)
          (funcall (publisher page))
          (collect (page-name page)))))


;; -- Unpublish --

;; Not implemented yet.



;; -- Published pages --

(defun published-pages (webapp)
  (iter (for (page-name nil) in-hashtable (dispatch-table (ensure-webapp webapp)))
        (collect page-name)))