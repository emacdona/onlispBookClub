(ql:quickload '("hunchentoot" "caveman2" "spinneret" "djula" "swank"))
(defvar *acceptor* (make-instance 'hunchentoot:easy-acceptor :port 8282))
(setf (hunchentoot:acceptor-document-root *acceptor*) #p".")

(defun main (args)
  (declare (ignore args))
  (start-web)
  ;; let the webserver run.
  ;; warning: hardcoded "hunchentoot".
  (handler-case (bt:join-thread (find-if (lambda (th)
                                            (search "hunchentoot" (bt:thread-name th)))
                                         (bt:all-threads)))
    ;; Catch a user's C-c
    (#+sbcl sb-sys:interactive-interrupt
      #+ccl  ccl:interrupt-signal-condition
      #+clisp system::simple-interrupt-condition
      #+ecl ext:interactive-interrupt
      #+allegro excl:interrupt-signal
      () (progn
           (format *error-output* "Aborting.~&")
           (stop-web)
           (uiop:quit)))
    (error (c) (format t "Woops, an unknown error occured:~&~a~&" c))))

(defun start-web()
  (format *error-output* "Starting web server~%")
  (hunchentoot:start *acceptor*)
  (format *error-output*  "Web server started~%"))

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

