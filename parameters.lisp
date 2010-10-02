(in-package :veil)

(declaim (optimize (speed 0) (debug 3)))

;;; ----------------------------------------------------------------------
;;; Conditions
;;; ----------------------------------------------------------------------

(define-condition http-parse-error ()
  ((raw-value :accessor raw-value :initarg :raw-value)
   (http-type :accessor http-type :initarg :http-type)))

(define-condition validation-error ()
  ((raw-value :accessor raw-value :initarg :raw-value)))



;;; ----------------------------------------------------------------------
;;; Lisp - HTML conversions
;;; ----------------------------------------------------------------------

(defparameter +html-true+ "true")
(defparameter +html-false+ "false")
(defparameter +html-null+ "")

(defgeneric lisp->html (value))

(defgeneric html->lisp (string type))


;; lisp to html

(defmethod lisp->html ((value (eql t)))
  +html-true+)

(defmethod lisp->html ((value (eql :null)))
  +html-null+)

(defmethod lisp->html ((value (eql nil)))
  +html-false+)

(defmethod lisp->html ((value integer))
  (format nil "~A" value))

(defmethod lisp->html ((value float))
  (format nil "~A" value))

(defmethod lisp->html ((value string))
  (format nil "~A" (escape-string value)))

(defmethod lisp->html ((value symbol))
  (format nil "~A" (string-downcase value)))


;; html to lisp

(defmethod html->lisp :around (value type)
  (cond ((null value)
         nil)
        ((string-equal value +html-null+)
         :null)
        (t
         (call-next-method (string-trim " " value) type))))

(defmethod html->lisp (value (type (eql 'string)))
  value)

(defmethod html->lisp (value (type (eql 'integer)))
  (handler-case (if (string-equal value +html-false+)
                    nil
                    (parse-integer value))
    (parse-error () (error 'http-parse-error
                           :http-type type
                           :raw-value value))))

(defmethod html->lisp (value (type (eql 'float)))
  (handler-case (if (string-equal value +html-false+)
                    nil
                    (parse-float value))
    (parse-error () (error 'http-parse-error
                           :http-type type
                           :raw-value value))))

(defmethod html->lisp (value (type (eql 'boolean)))
  (cond ((string-equal value +html-true+)  t)
        ((string-equal value +html-false+) nil)
        (t (error 'http-parse-error
                  :http-type type
                  :raw-value value))))

(defmethod html->lisp (value (type (eql 'symbol)))
  (intern (string-upcase value)))



;;; ----------------------------------------------------------------------
;;; HTTP parameters
;;; ----------------------------------------------------------------------

(defclass http-parameter ()
  ((name       :accessor name       :initarg :name)
   (key        :accessor key        :initarg :key)
   (page       :accessor page       :initarg :page)
   (lisp-type  :accessor lisp-type  :initarg :lisp-type)
   (vfn        :accessor vfn        :initarg :vfn)
   (vargs      :accessor vargs      :initarg :vargs)
   (val        :accessor val        :initarg :val)
   (raw        :accessor raw        :initarg :raw)
   (validp     :accessor validp     :initarg :validp)
   (error-type :accessor error-type :initarg :error-type)
   (requiredp  :accessor requiredp  :initarg :requiredp)
   (suppliedp  :accessor suppliedp  :initarg :suppliedp)))



;;; ----------------------------------------------------------------------
;;; run-time (request-time) parameter binding
;;; ----------------------------------------------------------------------

(defun find-params (page names)
  ;; Beware, future self: We have to iterate instead of using
  ;; remove-if-not or something similar because we have to keep the
  ;; order of the parameters unchanged
  (let ((parameters (parameters page)))
    (iter (for n in names)
          (collect (find n parameters :key #'name)))))

(defun parse-parameter (p raw)
  (handler-case (let ((parsed (html->lisp raw (lisp-type p))))
                  (if (and (null parsed)
                           (not (eql 'boolean (lisp-type p))))
                      (setf (raw p) raw
                            (val p) nil
                            (validp p) (not (requiredp p))
                            (suppliedp p) nil
                            (error-type p) nil)
                      (setf (raw p) raw
                            (val p) parsed
                            (validp p) t
                            (suppliedp p) t
                            (error-type p) nil)))
    (http-parse-error ()
      (setf (raw p) raw
            ;; leave val slot unbound
            (validp p) nil
            (suppliedp p) (if (null raw) nil t)
            (error-type p) :parse-error))))

(defun validate-parameter (p)
  (let ((pargs (find-params (page p) (vargs p))))
    (when (and (every #'suppliedp pargs)
               (every #'validp pargs))
      (let ((error-type (apply (vfn p) (mapcar #'val pargs))))
        (when error-type
          (slot-makunbound p 'val)
          (setf (validp p) nil)
          (setf (error-type p) error-type))))))

(defun set-parameters (page &optional query-string)
  (let ((query-alist (group-duplicate-keys (if (boundp '*request*)
                                               ;; normal behaviour
                                               (if (eql (request-type page) :get)
                                                   (get-parameters*)
                                                   (post-parameters*))
                                               ;; useful for debugging
                                               (parse-query-string query-string)))))
    (iter (for p in (parameters page))
          (for raw = (cdr (assoc (string-downcase (name p)) query-alist :test #'string-equal)))
          (parse-parameter p raw))
    (iter (for p in (parameters page))
          (validate-parameter p))))



;;; ------------------------------------------------------------
;;; Utilities
;;; ------------------------------------------------------------

(defun find-parameter (name &optional (page *page*))
  (find name (parameters page) :key (if (keywordp name) #'key #'name)))

(defun val* (param)
  (cond
    ;; parameter is null or it is not supplied: return nil
    ((or (null param) (not (suppliedp param)))
     nil)
    ;; parameter supplied but erroneous: return raw
    ((not (validp param))
     (raw param))
    ;; parameter supplied and ok: return val
    (t (val param))))




;; (defun bind-parameter! (p raw)
;;   (handler-case (let ((parsed (parse-raw raw (lisp-type p))))
;;                   (cond
;;                     ;; parameter not supplied
;;                     ((null raw)
;;                      (setf (val p) nil
;;                            (raw p) raw
;;                            (suppliedp p) nil))
;;                     ;; parameter supplied
;;                     ;; Exception: we accept boolean parameters with parsed value = NIL
;;                     ((or (funcall (vfn p) parsed)
;;                          (and (eql 'boolean (lisp-type p))
;;                               (member parsed '(t nil))))
;;                      (setf (val p) parsed
;;                            (raw p) raw
;;                            (validp p) t
;;                            (suppliedp p) t))
;;                     ;; parameter supplied but it is invalid
;;                     (t (error 'validation-error
;;                               :raw-value raw))))
;;     (validation-error ()
;;       (slot-makunbound p 'val)
;;       (setf (raw p) raw
;;             (validp p) nil
;;             (suppliedp p) t))))
