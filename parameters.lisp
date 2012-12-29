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


(defgeneric lisp->urlenc (value))
(defgeneric urlenc->lisp (string type))


(defmethod lisp->urlenc ((value (eql nil)))
  +urlenc-false+)

(defmethod lisp->urlenc ((value (eql t)))
  +urlenc-true+)

(defmethod lisp->urlenc ((value (eql :null)))
  +urlenc-null+)

(defmethod lisp->urlenc (value)
  (url-encode (format nil "~A" value)))


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

(defparameter *parameters* nil
  "This is bound within the body of every page to the list of parsed
  http-parameter objects")

(defclass http-parameter-attributes ()
  ((parameter-name :accessor parameter-name :initarg :parameter-name)
   (parameter-key  :accessor parameter-key  :initarg :parameter-key)
   (page           :accessor page           :initarg :page)
   (lisp-type      :accessor lisp-type      :initarg :lisp-type)
   (vfn            :accessor vfn            :initarg :vfn)
   (vargs          :accessor vargs          :initarg :vargs)
   (requiredp      :accessor requiredp      :initarg :requiredp)))

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
                                     :error-type (if (not (requiredp attr))
                                                     nil
                                                     :required-parameter-not-supplied))
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
                     :val nil
                     :validp nil
                     :suppliedp (if (null raw) nil t)
                     :error-type :parse-error))))

(defun validate-parameter (p parameters)
  (flet ((find-parameters (names)
           (loop for n in names
                 collect (find-parameter n parameters))))
    (let* ((attr (attributes p))
           (pargs (find-parameters (vargs attr))))
      (when (and (every #'suppliedp pargs)
                 (every #'validp pargs))
        (let ((error-type (apply (vfn attr) (mapcar #'val pargs))))
          (when error-type
            (setf (val p) nil)
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
            (loop for attr in (parameter-attributes page)
                  for raw = (cdr (assoc (string-downcase (parameter-name attr))
                                        query-alist
                                        :test #'string-equal))
                  collect (parse-parameter attr raw))))
      (dolist (p parameters)
        (validate-parameter p parameters))
      parameters)))



;;; ------------------------------------------------------------
;;; Exported parameter utilities
;;; ------------------------------------------------------------

(defun validate-parameters (chk-fn &rest parameters)
  "A posteriori parameter validation"
  (when (and (some #'suppliedp parameters)
             (every #'validp parameters))
    (when-let (error-type (apply chk-fn (mapcar #'val parameters)))
      (mapc (lambda (p)
              (setf (val p) nil)
              (setf (validp p) nil)
              (setf (error-type p) error-type))
            parameters))))

(defun find-parameter (name &optional (parameters *parameters*))
  (find name parameters :key (compose #'parameter-name #'attributes)))
