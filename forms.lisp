(in-package :widgets)

(declaim (optimize (speed 0) (debug 3)))


;;; ------------------------------------------------------------
;;; Macros
;;; ------------------------------------------------------------

(defmacro defhtml (name args &body body)
  `(defun ,name (&key ,@args)
     (lambda (&key ,@(mapcar #'list args args))
       ,@body)))

(defmacro html ((&rest args) &body body)
  `(lambda (,@args)
     (with-html
       ,@body)))

(defmacro render (html &rest args)
  (if (atom html)
      `(funcall (funcall ',html) ,@args)
      `(funcall ,html ,@args)))

(defun render (html &rest args)
  (cond ((functionp html)
         (apply html args))
        (symbolp html)))


(defmethod rendier ((html )))

(defmethod render* ((html function) &rest args)
  (apply html args))


(defsnippet submit2 (label name value style disabledp invisiblep)
  (if invisiblep
      (with-html "")
      (with-html
        (:button :class style
                 :type "submit"
                 :name (if name (string-downcase name) nil)
                 :value (if value (lisp->html value) nil)
                 :disabled disabledp
                 (str
                  (cond ((null label) nil)
                        ((functionp label)
                         (funcall label))
                        (t label)))))))
(defsnippet ok2 (invisiblep)
  (render submit2
          :invisiblep invisiblep
          :name "ok"
          :value "true"
          :label (lambda ()
                   (with-html
                     (:img :src "img/tick.png")))))


(defsnippet config-row ()
  (funcall (selector))
  (funcall (ok2)))

;;; ------------------------------------------------------------
;;; Forms
;;; ------------------------------------------------------------

(defsnippet form (submit-page hidden body)
  (let ((page (find-page submit-page)))
    (with-html
      (:form :method (request-type page)
             :action (base-url page)
             (iter (for key in hidden by #'cddr)
                   (for val in (rest hidden) by #'cddr)
                   (with-html
                     (:input :type "hidden"
                             :id (string-downcase key)
                             :class "display-none"
                             :name (string-downcase key)
                             :value (lisp->html val))))
             (funcall body)))))






;;; ------------------------------------------------------------
;;; Cells -- Non-CRUD
;;; ------------------------------------------------------------

(defclass cell (widget)
  ((style :accessor style :initarg :style)
   (value :accessor value :initarg :value)))

(defmethod render ((cell cell) &key)
  (with-html
    (str (lisp-to-html (value cell)))))



;;; ------------------------------------------------------------
;;; Cells -- CRUD
;;; ------------------------------------------------------------

(defclass cell-crud (cell) 
  ((visiblep :accessor visiblep :initarg :visiblep)))


;;; Selector

(defclass cell-selector (cell-crud)
  ((href-enabled  :accessor href-enabled  :initarg :href-enabled)
   (href-disabled :accessor href-disabled :initarg :href-disabled)))

(defmethod render ((cell cell-selector) &key)
  (with-html 
    (if (visiblep cell)
        (htm (:a :href href-enabled
                 (htm (:img :src (url "img/bullet_red.png")))))
        (htm (:a :href href-disabled
                 (htm (:img :src (url "img/bullet_blue.png"))))))))

(defmethod selector-href ((row row-crud))
  (let ((table (table row)))
    (apply (main-page table)
           (if barep
               (filter-params table)
               (append (id-data row) (filter-params table))))))

(defmethod input-box-style ((row row-crud))
  (let ((p (find (name cell) (params (table (row cell))) :key #'name)))
    (if (or (null p) (validp p)) "" "attention")))



;;; Textbox

(defun textbox ()
  (lambda (name &key id style readonlyp disabledp passwordp value invisiblep)
    (let ((computed-id (or id (string-downcase name)))) 
      (if invisiblep
          (with-html
            "")
          (with-html
            (:input :id computed-id ;; is it necessary that id has a value?
                    :class style
                    :type (if passwordp "password" "text")
                    :name (string-downcase name)
                    :value (lisp->html (or value :null))
                    :readonly readonlyp
                    :disabled disabledp))))))


;;; Dropdown

(defun dropdown ()
  (html (name pairs &key style selected readonlyp disabledp invisiblep)
    (if invisiblep
        (htm "")
        (htm
         (:select :id (string-downcase name)
                  :class style
                  :name (string-downcase name)
                  :disabled disabledp
                  (iter (for (label value) in label-value-alist) 
                        (htm (:option :value (lisp-to-html value)
                                      :selected (equal value selected) 
                                      :readonly readonlyp 
                                      (str (lisp-to-html label))))))))))

;; Submit

(defun submit (&key
               default-label
               default-name
               default-value
               default-style
               default-disabledp
               default-invisiblep)
  (lambda (&key
           (label default-label)
           (name default-name)
           (value default-value)
           (style default-style)
           (disabledp default-disabledp)
           (invisiblep default-invisiblep))
    (if invisiblep
        (with-html "")
        (with-html
         (:button :class style
                  :type "submit"
                  :name (if name (string-downcase name) nil)
                  :value (if value (lisp->html value) nil)
                  :disabled disabledp
                  (str
                   (cond ((null label) nil)
                         ((functionp label)
                          (funcall label))
                         (t label))))))))


;; Ok & Cancel

(defun ok (&key default-invisiblep)
  (lambda (&key (invisiblep default-invisiblep))
    (funcall (submit)
             :invisiblep invisiblep
             :label (lambda ()
                     (with-html
                       (:img :src "img/tick.png"))))))

(defun cancel (default-href)
  (html (&key (href default-href) invisiblep)
    (if invisiblep
        (htm "")
        (htm
         (:a :href href
             (:img :src "img/cancel.png"))))))




(defsnippet submit2 (label name value style disabledp invisiblep)
  (if invisiblep
      (with-html "")
      (with-html
        (:button :class style
                 :type "submit"
                 :name (if name (string-downcase name) nil)
                 :value (if value (lisp->html value) nil)
                 :disabled disabledp
                 (str
                  (cond ((null label) nil)
                        ((functionp label)
                         (funcall label))
                        (t label)))))))
(defsnippet ok2 (invisiblep)
  (render submit2
          :invisiblep invisiblep
          :name "ok"
          :value "true"
          :label (lambda ()
                   (with-html
                     (:img :src "img/tick.png")))))





























;;; ------------------------------------------------------------
;;; Page interface mixin
;;; ------------------------------------------------------------

(defclass page-interface-mixin ()
  ((params       :accessor params       :initarg :params)
   (id-keys      :accessor id-keys      :initarg :id-keys)
   (payload-keys :accessor payload-keys :initarg :payload-keys) 
   (filter-keys  :accessor filter-keys  :initarg :filter-keys)
   (aux-keys     :accessor aux-keys     :initarg :aux-keys)))

(defun params->plist (bag params) 
  (mapcan (lambda (key)
            (let ((par (find key params :key #'name)))
              (list key par)))
          bag))

(defgeneric id-params (obj))

(defmethod id-params ((obj page-interface-mixin))
  (plist-map-vals #'val*
                  (params->plist (id-keys obj) (params obj))))


(defgeneric payload-params (obj))

(defmethod payload-params ((obj page-interface-mixin))
  (plist-map-vals #'val*
                  (params->plist (payload-keys obj) (params obj))))


(defgeneric filter-params (obj))

(defmethod filter-params ((obj page-interface-mixin))
  (plist-map-vals #'val*
                  (params->plist (filter-keys obj) (params obj))))

(defgeneric aux-params (obj))

(defmethod aux-params ((obj page-interface-mixin))
  (plist-map-vals #'val*
                  (params->plist (aux-keys obj) (params obj))))


;;; ------------------------------------------------------------
;;; CRUD mixin  
;;; ------------------------------------------------------------

(defclass crud-mixin ()
  ((operation    :accessor operation    :initarg :operation)
   (main-page    :accessor main-page    :initarg :main-page) 
   (submit-pages :accessor submit-pages :initarg :submit-pages)))


(defgeneric submit-page (obj))

(defmethod submit-page ((obj crud-mixin))
  (getf (submit-pages obj) (operation obj)))


;;; ------------------------------------------------------------
;;; Tables 
;;; ------------------------------------------------------------

(defclass table (widget)
  ((header      :accessor header      :initarg :header)
   (styles      :accessor styles      :initarg :styles) 
   (data-fn     :accessor data-fn     :initarg :data-fn) 
   (cells-fn    :accessor cells-fn    :initarg :cells-fn)
   (tbody-class :accessor tbody-class :initform 'tbody)))

(defclass table-crud (table page-interface-mixin crud-mixin)
  (tbody-class :accessor tbody-class :initform 'tbody-crud))

(defmethod render ((table table) &key)
  (with-html
    (:table :id (name table)
            :class (getf (styles table) :table)
            ;; header
            (:thead 
             (:tr (plist-do (lambda (key label) 
                              (htm (:th :class (getf (getf (styles table) :header) key)
                                        (str label))))
                            (header table))))
            ;; body 
            (render (make-instance (tbody-class table)
                                   :table table
                                   :style (getf (styles table) :tbody)
                                   :data (apply (data-fn table)
                                                (filter-params table)))))))

;;; Table body

(defclass tbody ()
  ((table :accessor table :initarg :table)
   (data  :accessor data  :initarg :data) 
   (style :accessor style :initarg :style)))

(defmethod render ((tbody tbody) &key)
  (let ((row-style (getf (styles (table tbody)) :inactive-row)))
    (iter (for data-row in (data tbody))
          (render (make-instance row-class
                                 :table (table tbody)
                                 :data data-row
                                 :style row-style)))))

(defclass tbody-crud ()
  ())

(defmethod render ((tbody tbody-crud) &key)
  (let ((active-row-style (getf (styles (table tbody)) :active-row))
        (inactive-row-style (getf (styles (table tbody)) :inactive-row))
        (attention-row-style (getf (styles (table tbody)) :attention-row))
        (table (table tbody))) 
    (case (operation table)
      (:view 
       (iter (for data-row in (data tbody)) 
             (render (make-instance 'row-crud
                                    :table table
                                    :style (if (active-row-p table data-row)
                                               active-row-style
                                               inactive-row-style)
                                    :data data-row))))
      (:create
       (let ((rows (cons (make-instance 'row-crud
                                        :table table
                                        :style active-row-style
                                        :data (append (id-params table)
                                                      (payload-params table)))
                         (iter (for data-row in (data tbody)) 
                               (collect (make-instance 'row-crud
                                                       :table table
                                                       :style inactive-row-style
                                                       :data data-row))))))
         (render (make-form :submit-page (submit-page table)
                            :hidden (append (id-params table)
                                            (filter-params table)
                                            (aux-params table))
                            :body rows))))
      ((:update :delete) 
       (let ((rows
              (iter (for data-row in (data tbody))
                    (let* ((activep (active-row-p table data-row))
                           (row-style (if activep
                                          (if (eql (operation table) :delete)
                                              attention-row-style
                                              active-row-style)
                                          inactive-row-style))
                           (data (if activep
                                     (plist-union (append (id-params table)
                                                          (payload-params table))
                                                  data-row)
                                     data-row)))
                      (collect (make-instance 'row-crud
                                              :table table 
                                              :style row-style
                                              :data data)))))) 
         (render (make-form :submit-page (submit-page table) 
                            :hidden (append (id-params table)
                                            (filter-params table)
                                            (aux-params table))
                            :body rows)))))))

;;; Rows

(defclass row (widget)
  ((table :accessor table :initarg :table)
   (data  :accessor data  :initarg :data) 
   (style :accessor style :initarg :style)))

(defmethod render ((row row) &key)
  (let ((cells-list (funcall (cells-fn (table row)) row)))
    (with-html
      (:tr :class (style row)
           (dolist (c cells-list)
             (htm (:td (render c))))))))

(defclass row-crud (row)
  ())

(defgeneric id-data (row-crud))

(defmethod id-data ((row row-crud))
  (plist-collect (id-keys (table row)) (data row)))

(defgeneric payload-data (row-crud))

(defmethod payload-data ((row row-crud))
  (plist-collect (payload-keys (table row)) (data row)))




;;; ------------------------------------------------------------
;;; Trees
;;; ------------------------------------------------------------

(defclass tree (table)
  ((styles      :accessor styles      :initarg :styles) 
   (data-fn     :accessor data-fn     :initarg :data-fn) 
   (cells-fn    :accessor cells-fn    :initarg :cells-fn)))

(defclass tree-crud (tree page-interface-mixin crud-mixin)
  ())

(defmethod render ((tree tree) &key) 
  (with-html
    (:div :id (name tree)
          :class (getf (styles tree) :tree) 
          ;; body
          (render (make-instance (tbody-class table)
                                 :table table
                                 :style (getf (styles table) :tbody)
                                 :data (apply (data-fn table)
                                              (filter-params table)))))))




;;; ------------------------------------------------------------
;;; Table body
;;; ------------------------------------------------------------

;;; Non-CRUD

(defclass tbody ()
  ((table :accessor table :initarg :table)
   (data  :accessor data  :initarg :data) 
   (style :accessor style :initarg :style)))

(defclass tbody-normal (tbody)
  ())

(defmethod render ((tbody tbody-normal) &key)
  (with-html
    ))


(defclass tbody-ul (tbody)
  ())

(defmethod render ((tbody tbody-ul) &key)
  (with-html
    (:ul :class (style tbody)
         (render-tbody tbody (row-class (table tbody))))))




;;; CRUD

(defclass tbody-normal-crud (tbody-normal)
  ())


(defmethod render ((tbody tbody-normal-crud) &key)
  (with-html
    (:tbody :style (style tbody)
            (render-tbody-crud tbody (row-class (table tbody))))))

(defclass tbody-ul-crud (tbody-ul)
  ())

(defmethod render ((tbody tbody-ul-crud) &key)
  (with-html
    (:ul :style (style tbody)
         (render-tbody-crud tbody (row-class (table tbody))))))





;;; ------------------------------------------------------------
;;; Rows
;;; ------------------------------------------------------------

;;; Non-CRUD

(defclass row (widget)
  ((table :accessor table :initarg :table)
   (style :accessor style :initarg :style) 
   (data  :accessor data  :initarg :data)))

(defclass row-normal (row)
  ())

(defmethod render ((row row-normal) &key)
  (let ((cells-list (funcall (cells-fn (table row)) row)))
    (with-html
      (:tr :class (style row)
           (render cells-list)))))

(defclass row-ul (row)
  ())

(defmethod render ((row row-ul) &key)
  (let ((cells-list (funcall (cells-fn (table row)) row)))
    (with-html
      (:li :class (style row)
           (render cells-list)))))

;;; CRUD

(defclass row-crud (row)
  ())

(defclass row-normal-crud (row-normal row-crud)
  ())

(defclass row-ul-crud (row-ul row-crud)
  ())


;;; Other row utilities

;;; Compare: id-data <--> id-params
;;; and payload-data <--> payload-params

(defgeneric id-data (row-crud))

(defmethod id-data ((row row-crud))
  (plist-collect (id-keys (table row)) (data row)))

(defgeneric payload-data (row-crud))

(defmethod payload-data ((row row-crud))
  (plist-collect (payload-keys (table row)) (data row)))




;;; ------------------------------------------------------------
;;; Utilities 
;;; ------------------------------------------------------------

(defun active-row-p (table data)
  (set-equal (plist-collect (id-keys table) data)
             (id-params table)
             :test #'equal))



;;; ------------------------------------------------------------
;;; Navigation bars
;;; ------------------------------------------------------------

(defmacro define-navbar (name (&rest arglist) (&key id div-style ul-style)
                         &body body) 
  (multiple-value-bind (items fns) (iter (for sexp in body)
                                         (destructuring-bind (sym href &rest forms) sexp
                                           (collect sym into items)
                                           (collect `(lambda (class)
                                                       (with-html
                                                         (:li (:a :class class
                                                                  :href ,href
                                                                  ,@forms)))) into fns)
                                           (finally (return (values items fns)))))
    `(defun ,name (active-item ,@arglist) 
       (with-html
         (:div :id ,id :class ,div-style
               (:ul :class ,ul-style 
                    (iter (for item in ',items)
                          (for fn in (list ,@fns))
                          (funcall fn (if (eql item active-item) "active" nil)))))))))

(defmacro define-menu (name (&rest args) (&key id div-style ul-style)
		       &body body)
  (with-gensyms (opt-list)
    (let ((options (iter (for (key fn-body) in body)
			 (collect key)
			 (collect `(lambda ,args
				     (declare (ignorable ,@args))
				     ,fn-body)))))
      `(defun ,name ,(append args `(&rest ,opt-list))
	 (let ((fns (list ,@options)))
	   (with-html
	     (:div :id ,id :class ,div-style
		   (:ul :class ,ul-style
			(iter (for key in ,opt-list)
			      (funcall (getf fns key) ,@args))))))))))


(defmacro define-errorbar (name (&key id div-style ul-style)
			   &body body)
  (let ((arglist (mapcar #'first body)))
    `(defun ,name ,arglist
       (unless (every #'validp (list ,@arglist))
	 (with-html
	   (:div :id ,id :class ,div-style
		 (:ul :class ,ul-style 
		      ,@(iter (for (arg msg) in body)
                              (collect `(unless (validp ,arg)
                                          (htm (:li ,msg))))))))))))

(defmacro filter (action id filter disabledp)
  `(with-form (,action :id ,id)
     (with-html
       (:p "Φίλτρο: " (textbox 'filter :value ,filter :disabledp ,disabledp) (ok-button)))))























