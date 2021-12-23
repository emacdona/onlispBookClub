(ql:quickload '("hunchentoot" "caveman2" "spinneret" "djula"))
(defvar *acceptor* (make-instance 'hunchentoot:easy-acceptor :port 4242))
(setf (hunchentoot:acceptor-document-root *acceptor*) #p".")

(defun start-web()
  (hunchentoot:start *acceptor*))

(defun stop-web()
  (hunchentoot:stop *acceptor*))


(defun hello()
  (format nil "Hello, it works!"))

(push
 (hunchentoot:create-prefix-dispatcher "/hello.html" #'hello)
 hunchentoot:*dispatch-table*)

(hunchentoot:define-easy-handler (say-hi :uri "/hi") ()
  (setf (hunchentoot:content-type*) "text/html")
  (spinneret:with-html-string
    (:doctype)
    (:html
     (:head
      (:title "Hello World"))
     (:body
      (:h1 "Hello World")
      (:p "Hello!"))))
  )

(hunchentoot:define-easy-handler (say-yo :uri "/yo") (name)
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "Hey~@[ ~A~]!" name))

(hunchentoot:define-easy-handler (say-bye :uri "/bye") (name)
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "Bye~@[ ~A~]!" name))

