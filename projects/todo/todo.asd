;;;; todo.asd

(ql:quickload :alexandria)
(ql:quickload :cl-ppcre)

(asdf:defsystem #:todo
  :description "Describe todo here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "todo")))
