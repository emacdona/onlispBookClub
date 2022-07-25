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

(defmacro aget (key alist)
  `(cdr (assoc ,key ,alist)))

(defun unit-meta(unit)
  (aget unit *unit-db*))

(defun can-prefix(unit)
  (aget :can-prefix (unit-meta unit)))

(defun valid-unit-types(unit)
  (aget :type (unit-meta unit)))

(defun default-unit-type(unit)
  (let ((valid-types (valid-unit-types unit))
        (default-type (aget :default-type (unit-meta unit))))
    (cond (default-type default-type)
          ((= (length valid-types) 1) (car valid-types))
          (t nil))))

(defun units()
  (mapcar (lambda (x) (car x)) *unit-db*))

(defun unit-types(&optional (name nil namep))
  (if namep
      (valid-unit-types name)
      (remove-duplicates
       (apply #'append
              (mapcar (lambda (x) (aget :type (cdr x)))
                      *unit-db*)))))

(defclass unit ()
  ((name
    :initarg :name
    :initform nil)
   (type
    :initarg :type
    :initform nil)
   (prefix
    :initarg :prefix
    :initform nil)))

(defmethod initialize-instance :after ((u unit) &key)
  (let* ((name (slot-value u 'name))
         (type (slot-value u 'type))
         (default-type (default-unit-type name))
         (valid-types (unit-types name)))
    (cond
      ((not (member name (units)))
       (error (format nil "Invalid name. Must be one of: ~s" (units))))
      ((and (not type) (not default-type))
       (error (format nil "Ambiguous unit. You MUST specify a type. One of: ~s" valid-types)))
      ((and type (not (member type valid-types)))
       (error (format nil "The specified quantity type does not apply for ~s. Try one of ~s." name valid-types)))
      ((not type)
       (setf (slot-value u 'type) default-type)))))
