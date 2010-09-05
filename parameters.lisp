(in-package :veil)

(declaim (optimize (speed 0) (debug 3)))

;;; ----------------------------------------------------------------------
;;; Conditions    
;;; ----------------------------------------------------------------------

(define-condition http-parse-error ()
  ((raw-value   :accessor raw-value   :initarg :raw-value)
   (http-type   :accessor http-type   :initarg :http-type)))

(define-condition validation-error ()
  ((raw-value  :accessor raw-value  :initarg :raw-value)))



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
                  (break)
                  (cond
                    ;; parameter not supplied
                    ((null raw) 
                     (setf (val p) nil
                           (raw p) nil
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
                                               (if (eql (request-type page) :get)
                                                   #'hunchentoot:get-parameters*
                                                   #'hunchentoot:post-parameters*)
                                               (parse-query-string query-string)))))
    ;; First, bind parameters and check with their own validators
    
    (iter (for p in (parameters page))
          ;; See parse-query-string comment for the assoc comparison & make-keyword
          (for raw = (cdr (assoc (make-keyword (name p)) query-alist)))
          (bind-parameter! p raw))
    ;; Then, check again using the page validators
    (iter (for v in (validators page))
          (for fn = (car v))
          (for pnames = (cdr v))
          (for params = (remove-if-not (lambda (x)
                                         (member x pnames))
                                       (parameters page)
                                       :key #'name))
          
          (unless (and (every #'validp params)
                       (apply fn (mapcar #'val params)))
            (mapc #'unbind-parameter! params)))))



































;; (defun foo (string)
;;   (conc "foo" string))

;; (defclass )


;; (defclass input (input-elements)
;;   ((name      :accessor name      :initarg :name)
;;    (style     :accessor style     :initarg :style)
;;    (disabledp :accessor disabledp :initarg :disabledp)
;;    (passwordp :accessor passwordp :initarg :passwordp)
;;    (value     :accessor value     :initarg :value)))

;; (defun input (name &key style readonlyp disabledp passwordp value)
;;   )


;; (defmethod render ((obj textbox))
;;   (with-html
;;     (:input :id (string-downcase name)
;;             :class style
;;             :type (if passwordp "password" "text")
;;             :name (string-downcase name)
;;             :value (lisp-to-html (or value :null))
;;             :readonly readonlyp
;;             :disabled disabledp)))







;; ;;; Selector

;; (defclass cell-selector (cell-crud)
;;   ((href-enabled  :accessor href-enabled  :initarg :href-enabled)
;;    (href-disabled :accessor href-disabled :initarg :href-disabled)))

;; (defmethod render ((cell cell-selector) &key)
;;   (with-html 
;;     (if (enabledp cell)
;;         (htm (:a :href href-enabled
;;                  (htm (:img :src (url "img/bullet_red.png")))))
;;         (htm (:a :href href-disabled
;;                  (htm (:img :src (url "img/bullet_blue.png"))))))))

;; (defmethod selector-href ((row row-crud))
;;   (let ((table (table row)))
;;     (apply (main-page table)
;;            (if barep
;;                (filter-params table)
;;                (append (id-data row) (filter-params table))))))

;; (defmethod input-box-style ((row row-crud))
;;   (let ((p (find (name cell) (params (table (row cell))) :key #'name)))
;;     (if (or (null p) (validp p)) "" "attention")))



;; ;;; Textbox

;; (labels ((param (cell)
;;            )
;;          (box-style ()
;;            )))

;; (defclass cell-textbox (cell-crud)
;;   ())

;; (defmethod render ((cell cell-textbox) &key) 
;;   (if (enabledp cell) 
;;       (with-html
;;         (textbox (name cell) 
;;                  :value (value cell)
;;                  :style (style cell)))
;;       (call-next-method)))



;; ;;; Dropdown

;; (defclass cell-dropdown (cell-crud)
;;   ((pairs :accessor pairs :initarg :pairs)))

;; (defmethod render ((cell cell-dropdown) &key)
;;   (if (enabledp cell)
;;       (with-html
;;         (dropdown (name cell)
;;                   (pairs cell)
;;                   :style style
;;                   :selected (value cell)))
;;       (call-next-method)))


;; ;; Submit

;; (defclass cell-submit (cell-crud)
;;   ())

;; (defmethod render ((cell cell-submit) &key)
;;   (if (enabledp cell)
;;       (with-html
;;         (:button :type "submit"
;;                  (:img :src (url "img/tick.png"))))
;;       (with-html
;;         "")))


;; ;; Cancel

;; (defclass cell-cancel (cell-crud)
;;   ((href :accessor href :initarg :href)))

;; (defmethod render ((cell cell-cancel) &key) 
;;   (if (enabledp cell)
;;       (with-html
;;         (:a :href (href cell)
;;             (:img :src (url "img/cancel.png"))))
;;       (with-html
;;         "")))
