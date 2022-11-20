(in-package #:todo)

(defvar *unit-db*
  '((:tsp .
     ((:type . (:volume))))
    (:tbsp .
     ((:type . (:volume))))
    (:cup .
     ((:type . (:volume))))
    (:ounce .
     ((:type . (:volume :weight))))
    (:pint .
     ((:type . (:volume))))
    (:quart .
     ((:type . (:volume))))
    (:gallon .
     ((:type . (:volume))))
    (:liter .
     ((:type . (:volume))
      (:can-prefix . t)))
    (:pound .
     ((:type . (:weight :mass))
      (:default-type . :weight)))
    (:stone .
     ((:type . (:weight))))
    (:gram .
     ((:type . (:mass))
      (:can-prefix . t)))))

(defvar *unit-prefixes*
  (alist-hash-table
   '((:kilo . 1000)
     (:hecto . 100)
     (:deca . 10)
     (:deci . 1/10)
     (:centi . 1/100)
     (:milli . 1/1000))))

(defun aget (key alist)
  "Get the value associated with key in assoc list."
  (cdr (assoc key alist)))

(defun unit-meta(unit)
  "Get the metadata associated with unit in the unit database."
  (aget unit *unit-db*))

(defun can-prefix(unit)
  "Can the given unit be prefixed?"
  (aget :can-prefix (unit-meta unit)))

(defun valid-unit-types(unit)
  "Get the valid unit types for unit."
  (aget :type (unit-meta unit)))

(defun default-unit-type(unit)
  "Get the default unit type for unit -- IF it has one. nil otherwise."
  (let ((valid-types (valid-unit-types unit))
        (default-type (aget :default-type (unit-meta unit))))
    (cond (default-type default-type)
          ((= (length valid-types) 1) (car valid-types))
          (t nil))))

(defun units()
  "Get all unit names."
  (mapcar (lambda (x) (car x)) *unit-db*))

(defun unit-types(&optional (name nil namep))
  "Get the valid unit types for unit named by name -- if specified. Otherwise, get all unit types."
  (if namep
      (valid-unit-types name)
      (remove-duplicates
       (apply #'append
              (mapcar (lambda (x) (aget :type (cdr x)))
                      *unit-db*)))))

(class/std json-deserializeable-prototype
  lisp-package
  lisp-class
  lisp-superclasses)

(class/std json-deserializeable
  prototype)

(defclass unit (json-deserializeable)
  ((name :initarg :name :initform nil)
   (type :initarg :type :initform nil)
   (prefix :initarg :prefix :initform nil)))

(defmethod initialize-instance :before ((u unit) &key name type))
;(defmethod initialize-instance :before ((u unit) &key name type)
;  (let* ((default-type (default-unit-type name))
;         (valid-types (unit-types name)))
;    (cond
;      ((not (member name (units)))
;       (error "Invalid name: ~s. Must be one of: ~s" name (units)))
;      ((and (not type) (not default-type))
;       (error "Ambiguous unit. You MUST specify a type. One of: ~s" valid-types))
;      ((and type (not (member type valid-types)))
;       (error "The specified quantity type does not apply for ~s. Try one of ~s." name valid-types)))))

(defmethod initialize-instance :after ((u unit) &key)
  (progn
    (format t "Running after!~%")
    (with-slots (name type) u
      (if (null type)
          (setf type (default-unit-type name))))))

(defgeneric encode (unit))

(defmethod encode ((unit unit))
  (with-slots (prototype) unit
    (setf prototype (make-instance 'json-deserializeable-prototype
                                   :lisp-package 'todo
                                   :lisp-class 'unit))
    (encode-json-to-string unit)))

(defun char-concat (string char)
  (concatenate 'string string
          (concatenate 'list "" '(char))))

(defun string-to-keyword-handlers (member-names)
  (let ((convert-string-to-keyword nil)
        (orig-object-key-handler cl-json:*object-key-handler*)
        (orig-beginning-of-string-handler cl-json:*beginning-of-string-handler*)
        (orig-string-char-handler cl-json:*string-char-handler*)
        (orig-end-of-string-handler cl-json:*end-of-string-handler*))
    (labels
        ((object-key-handler (key)
           (if (memg))
           ))
        )))

(defun decode (string)
  (with-decoder-simple-clos-semantics
    (let ((convert-string-to-keyword nil)
          (orig-object-key-handler cl-json:*object-key-handler*)
          (orig-object-value-handler cl-json:*object-value-handler*)
          (orig-end-of-string-handler cl-json:*end-of-string-handler*)
          )
      (labels
          ((object-key (key)
             (if (member-if (lambda (x)
                              (progn
                                (string= x key)))
                            '("name" "type"))
                 (progn
                   (format t "~s is a string I know!~%" key)
                   (setf convert-string-to-keyword t)
                   (let ((k (funcall orig-object-key-handler key)))
                     (format t "Value for key that I'm returning: ~s~%" key)
                     key)
                 )
                 (progn
                   (funcall orig-object-key-handler key)
                   ))
             )
           (object-value (value)
             (progn
               ;;(setf convert-string-to-keword nil)
               (funcall orig-object-value-handler value)
               )
             )
;           (object-value (value)
;             (if convert-string-to-keyword
;                 (progn
;                   (format t "Converting ~s to symbol.~%" value)
;                   (setf convert-string-to-keyword nil)
;                   (intern (string-upcase value) "KEYWORD")
;                   )
;                 (progn
;                   (funcall orig-object-value-handler value)
;                   ))
                                        ;             )
           )
        (json:bind-custom-vars
            (:object-key #'object-key
             :object-value #'object-value
             :end-of-string (lambda ()
                              (if convert-string-to-keyword
                                  (progn
                                    (format t "fuck!~%")
                                    (setf convert-string-to-keyword nil)
                                    :KEY
                                    )
                                  (progn
                                    (format t "fuck no!~%")
                                    (funcall orig-end-of-string-handler))))
             )
          (decode-json-from-string string)
          )
        )
      )
    )
  )

(defun simple-decode (string)
  (with-decoder-simple-clos-semantics
    (decode-json-from-string string)))
