;;;; todo.lisp

(in-package #:todo)

(defclass quantity ()
  (;; The amount of the quantity
   (scalar
    :initarg :scalar)

   ;; Symbol representing unit. Used by conversion algorithm.
   (unit
    :initarg :unit)))

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

