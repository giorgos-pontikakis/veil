(in-package :veil)



;;; ----------------------------------------------------------------------
;;; Conditions
;;; ----------------------------------------------------------------------

(define-condition http-parse-error ()
  ((raw-value :accessor raw-value :initarg :raw-value)
   (http-type :accessor http-type :initarg :http-type)))

(define-condition validation-error ()
  ((raw-value :accessor raw-value :initarg :raw-value)))



;;; ----------------------------------------------------------------------
;;; Lisp - HTML and Lisp - URL encoding conversions
;;; ----------------------------------------------------------------------

(defparameter +html-true+ "true")
(defparameter +html-false+ "false")
(defparameter +html-null+ "")

(defgeneric lisp->html (value))

(defgeneric urlenc->lisp (string type))


;; lisp to html

(defmethod lisp->html ((value (eql t)))
  +html-true+)

(defmethod lisp->html ((value (eql :null)))
  +html-null+)

(defmethod lisp->html ((value (eql nil)))
  +html-false+)

(defmethod lisp->html ((value integer))
  (format nil "~D" value))

(defmethod lisp->html ((value rational))
  (format nil "~,2F" value))

(defmethod lisp->html ((value float))
  (format nil "~,4F" value))

(defmethod lisp->html ((value string))
  (escape-for-html (format nil "~A" value)))

(defmethod lisp->html ((value symbol))
  (escape-for-html (format nil "~A" (string-downcase value))))

(defmethod lisp->html ((value date))
  (multiple-value-bind (year month day) (decode-date value)
    (format nil "~A/~A/~A" day month year)))


;; urlenc to lisp and vice versa

(defun lisp->urlenc (value)
  (url-encode (cond ((null value) +html-false+)
                    ((eql value t) +html-true+)
                    ((eql value :null) +html-null+)
                    (t (format nil "~A" value)))))

(defmethod urlenc->lisp :around (value type)
  (cond ((null value)
         nil)
        ((string-equal value +html-null+)
         :null)
        ((string-equal value +html-false+)
         nil)
        (t
         (call-next-method (string-trim " " value) type))))

(defmethod urlenc->lisp (value (type (eql 'string)))
  value)

(defmethod urlenc->lisp (value (type (eql 'integer)))
  (handler-case (parse-integer value)
    (parse-error () (error 'http-parse-error
                           :http-type type
                           :raw-value value))))

(defmethod urlenc->lisp (value (type (eql 'float)))
  (handler-case (parse-float value)
    (parse-error () (error 'http-parse-error
                           :http-type type
                           :raw-value value))))

(defmethod urlenc->lisp (value (type (eql 'boolean)))
  (cond ((string-equal value +html-true+)  t)
        ((string-equal value +html-false+) nil)
        (t (error 'http-parse-error
                  :http-type type
                  :raw-value value))))

(defmethod urlenc->lisp (value (type (eql 'symbol)))
  (intern (string-upcase value)))

(defmethod urlenc->lisp (value (type (eql 'date)))
  (handler-case (if (string-equal value +html-false+)
                    nil
                    (parse-date value))
    (error () ;; match all errors
      (error 'http-parse-error
             :http-type type
             :raw-value value))))


;;; ----------------------------------------------------------------------
;;; HTTP parameters
;;; ----------------------------------------------------------------------

(defclass http-parameter ()
  ((attributes :accessor attributes :initarg :attributes)
   (val        :accessor val        :initarg :val)
   (raw        :accessor raw        :initarg :raw)
   (validp     :accessor validp     :initarg :validp)
   (error-type :accessor error-type :initarg :error-type)
   (suppliedp  :accessor suppliedp  :initarg :suppliedp)))



;;; ----------------------------------------------------------------------
;;; run-time (request-time) parameter binding
;;; ----------------------------------------------------------------------

(defun parse-parameter (attr raw)
  (handler-case (let ((parsed (urlenc->lisp raw (lisp-type attr))))
                  (if (null raw)
                      (make-instance 'http-parameter
                                     :attributes attr
                                     :raw raw
                                     :val nil
                                     :validp (not (requiredp attr))
                                     :suppliedp nil
                                     :error-type nil)
                      (make-instance 'http-parameter
                                     :attributes attr
                                     :raw raw
                                     :val parsed
                                     :validp t
                                     :suppliedp t
                                     :error-type nil)))
    (http-parse-error ()
      (make-instance 'http-parameter
                     :attributes attr
                     :raw raw
                     :val raw
                     :validp nil
                     :suppliedp (if (null raw) nil t)
                     :error-type :parse-error))))

(defun validate-parameter (p parameters)
  (flet ((find-params (names)
           (iter (for n in names)
                 (collect (find n parameters :key (compose #'name #'attributes))))))
    (let* ((attr (attributes p))
           (pargs (find-params (vargs attr))))
      (when (and (every #'suppliedp pargs)
                 (every #'validp pargs))
        (let ((error-type (apply (vfn attr) (mapcar #'val pargs))))
          (when error-type
            (setf (val p) (raw p))
            (setf (validp p) nil)
            (setf (error-type p) error-type)))))))

(defun parse-parameters (page &optional query-string)
  (let ((query-alist (group-duplicate-keys (if (boundp '*request*)
                                               ;; normal behaviour
                                               (if (eql (request-type page) :get)
                                                   (get-parameters*)
                                                   (post-parameters*))
                                               ;; useful for debugging
                                               (parse-query-string query-string)))))
    (let ((parameters
           (iter (for attr in (parameter-attributes page))
                 (for raw = (cdr (assoc (string-downcase (name attr))
                                        query-alist
                                        :test #'string-equal)))
                 (collect (parse-parameter attr raw)))))
      (dolist (p parameters)
        (validate-parameter p parameters))
      parameters)))



;;; ------------------------------------------------------------
;;; Exported utilities
;;; ------------------------------------------------------------

(defun validate-parameters (chk-fn &rest parameters)
  (when (and (some #'suppliedp parameters)
             (every #'validp parameters))
    (when-let (error-type (apply chk-fn (mapcar #'val parameters)))
      (mapc (lambda (p)
              (setf (val p) (raw p))
              (setf (validp p) nil)
              (setf (error-type p) error-type))
            parameters))))

(defun parse-date (value)
  (destructuring-bind (day month year) (mapcar #'parse-integer (split "-|/|\\." value))
    (encode-date (if (< year 1000) (+ year 2000) year)
                 month
                 day)))