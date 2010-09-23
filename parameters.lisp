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

(defmethod lisp->html ((value t))
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
         (call-next-method))))

(defmethod html->lisp (value (type (eql 'string)))
  (string-trim " " value))

(defmethod html->lisp (value (type (eql 'integer)))
  (let ((trimmed-value (string-trim " " value)))
    (handler-case (if (string-equal trimmed-value +html-false+)
                      nil
                      (parse-integer trimmed-value))
      (parse-error () (error 'http-parse-error
                             :http-type type
                             :raw-value trimmed-value)))))

(defmethod html->lisp (value (type (eql 'float)))
  (let ((trimmed-value (string-trim " " value)))
    (handler-case (if (string-equal trimmed-value +html-false+)
                      nil
                      (parse-float trimmed-value))
      (parse-error () (error 'http-parse-error
                             :http-type type
                             :raw-value trimmed-value)))))

(defmethod html->lisp (value (type (eql 'boolean)))
  (let ((trimmed-value (string-trim " " value)))
    (cond ((string-equal trimmed-value +html-true+)  t)
          ((string-equal trimmed-value +html-false+) nil) 
          (t (error 'http-parse-error
                    :http-type type
                    :raw-value trimmed-value)))))

(defmethod html->lisp (value (type (eql 'symbol)))
  (intern (string-upcase (string-trim " " value))))



;;; ----------------------------------------------------------------------
;;; HTTP parameters    
;;; ----------------------------------------------------------------------

(defclass http-parameter () 
  ((name      :accessor name      :initarg :name)
   (key       :accessor key       :initarg :key) 
   (lisp-type :accessor lisp-type :initarg :lisp-type) 
   (validator :accessor validator :initarg :validator)
   (requiredp :accessor requiredp :initarg :requiredp) 
   (val       :accessor val       :initarg :val)
   (raw       :accessor raw       :initarg :raw)
   (validp    :accessor validp    :initarg :validp)
   (suppliedp :accessor suppliedp :initarg :suppliedp)))



;;; ----------------------------------------------------------------------
;;; run-time (request-time) parameter binding
;;; ----------------------------------------------------------------------

(defun parse-raw (raw type)
  (handler-case (html->lisp raw type)
    (http-parse-error (c)
      (error 'validation-error 
	     :raw-value (raw-value c)))))

(defun bind-parameter! (p raw)
  (handler-case (let ((parsed (parse-raw raw (lisp-type p)))) 
                  (cond
                    ;; parameter not supplied or supplied but empty
                    ((or (null raw) (eql parsed :null))
                     (setf (val p) nil
                           (raw p) raw
                           (validp p) (not (requiredp p))
                           (suppliedp p) nil))
                    ;; parameter supplied and it is valid.
                    ;; Exception: we accept boolean parameters with parsed value = NIL 
                    ((or (funcall (or (validator p) #'identity) parsed)
                         (and (eql 'boolean (lisp-type p))
                              (member parsed '(t nil))))
                     (setf (val p) parsed
                           (raw p) raw
                           (validp p) t
                           (suppliedp p) t))
                    ;; parameter supplied but it is invalid
                    (t (error 'validation-error
                              :raw-value raw))))
    (validation-error ()
      (slot-makunbound p 'val)
      (setf (raw p) raw
            (validp p) nil
            (suppliedp p) t))))

(defun unbind-parameter! (p)
  (slot-makunbound p 'val)
  (setf (validp p) nil))

(defun bind-parameters! (page &optional query-string)
  (let ((query-alist (group-duplicate-keys (if (boundp '*request*)
                                               ;; normal behaviour
                                               (if (eql (request-type page) :get)
                                                   (get-parameters*)
                                                   (post-parameters*))
                                               ;; useful for debugging
                                               (parse-query-string query-string))))) 
    ;; First, bind parameters and check with their own validators 
    (iter (for p in (parameters page))
          (for raw = (cdr (assoc (string-downcase (name p)) query-alist :test #'string-equal))) 
          (bind-parameter! p raw))
    ;; Then, check again using the page validators
    (flet ((find-params (names) (remove-if-not (lambda (x)
                                                 (member x names))
                                               (parameters page)
                                               :key #'name)))
      (iter (for v in (validators page))
            (destructuring-bind (tested (fn &rest args) msg) v  ;; tested, args are names
              (let ((ptested (find-params tested))              ;; ptested, pargs are the 
                    (pargs (find-params args)))                 ;; corresponding parameters
                (unless (and (every #'validp pargs)
                             (apply fn (mapcar #'val pargs)))
                  (mapc #'unbind-parameter! ptested))))))))



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
