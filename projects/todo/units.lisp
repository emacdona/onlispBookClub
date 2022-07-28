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

(defclass unit ()
  ((name :initarg :name)
   (type :initarg :type :initform nil)
   (prefix :initarg :prefix)))

(defmethod initialize-instance :before ((u unit) &key name type)
  (let* ((default-type (default-unit-type name))
         (valid-types (unit-types name)))
    (cond
      ((not (member name (units)))
       (error "Invalid name. Must be one of: ~s" (units)))
      ((and (not type) (not default-type))
       (error "Ambiguous unit. You MUST specify a type. One of: ~s" valid-types))
      ((and type (not (member type valid-types)))
       (error "The specified quantity type does not apply for ~s. Try one of ~s." name valid-types)))))

(defmethod initialize-instance :after ((u unit) &key)
  (with-slots (name type) u
    (if (null type)
        (setf type (default-unit-type name)))))
