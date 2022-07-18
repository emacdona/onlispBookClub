;;;; package.lisp

(defpackage #:todo
  (:use #:cl
        #:alexandria
        #:cl-ppcre)
;;  (:local-nicknames (#:al #:alexandria)
;;                    (#:re #:cl-ppcre))
  (:export #:quantity
           #:ingredient
           #:recipe
           #:grocery-list))
