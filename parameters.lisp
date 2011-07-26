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
;;; Lisp <-> URL-encoding conversions
;;; ----------------------------------------------------------------------

(defparameter +urlenc-true+ "true")
(defparameter +urlenc-false+ "false")
(defparameter +urlenc-null+ "")


(defgeneric urlenc->lisp (string type))

(defun lisp->urlenc (value)
  (url-encode (cond ((null value) +urlenc-false+)
                    ((eql value t) +urlenc-true+)
                    ((eql value :null) +urlenc-null+)
                    (t (format nil "~A" value)))))

(defmethod urlenc->lisp :around (value type)
  (cond ((null value)
         nil)
        ((string-equal value +urlenc-null+)
         :null)
        ((string-equal value +urlenc-false+)
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
  (cond ((string-equal value +urlenc-true+)  t)
        ((string-equal value +urlenc-false+) nil)
        (t (error 'http-parse-error
                  :http-type type
                  :raw-value value))))

(defmethod urlenc->lisp (value (type (eql 'symbol)))
  (intern (string-upcase value)))



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
                 (collect (find n parameters :key (compose #'param-name #'attributes))))))
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
                 (for raw = (cdr (assoc (string-downcase (param-name attr))
                                        query-alist
                                        :test #'string-equal)))
                 (collect (parse-parameter attr raw)))))
      (dolist (p parameters)
        (validate-parameter p parameters))
      parameters)))



;;; ------------------------------------------------------------
;;; A posteriori parameter validation
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
