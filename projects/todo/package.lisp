;;;; package.lisp

(defpackage #:todo
  (:use #:cl)
  (:import-from #:alexandria
                #:alist-hash-table)
  (:import-from #:cl-ppcre)
  (:import-from #:yason)
  (:import-from #:json
                #:with-decoder-simple-clos-semantics
                #:encode-json-to-string
                #:decode-json-from-string)
  (:import-from #:defclass-std
                #:defclass/std
                #:class/std)
  (:export #:quantity
           #:ingredient
           #:recipe
           #:grocery-list))
