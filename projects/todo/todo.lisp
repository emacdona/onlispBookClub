;;;; todo.lisp

(in-package #:todo)

(defvar *unit-prefixes*
  (alexandria:alist-hash-table
   '((:kilo . 1000)
     (:hecto . 100)
     (:deca . 10)
     (:deci . 1/10)
     (:centi . 1/100)
     (:milli . 1/1000))))

(defvar *units*
  (alexandria:alist-hash-table
   '((:volume . (:tsp
                 :tbsp
                 :cup
                 :ounce
                 :pint
                 :quart
                 :gallon
                 :liter))
     (:weight . (:ounce
                 :gram
                 :pound
                 :stone)))))


(defclass quantity ()
  (;; The amount of the quantity
   (scalar
    :initarg :scalar)

   ;; Symbol representing unit. Used by conversion algorithm.
   (unit
    :initarg :unit)

   ;; Volume? Weight?
   (unit-type
    :initarg :unit-type)))

(defclass ingredient ()
  ((name
    :initarg :name)

   (quantity
    :initarg :quantity)))

;; A list of ingredients, each of which has a specified quantity
(defclass recipe ()
  ((ingredients
    :initarg :ingredients)))

;; A representation of a list of recipes that consolidates ingredients by aggregating
;; their quantities into a single grocery list item.
(defclass grocery-list ()
  ((recipes
    :initarg :recipes)))

