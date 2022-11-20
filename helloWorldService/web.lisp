(ql:quickload '("hunchentoot" "caveman2" "spinneret" "djula" "swank"))
(defvar *acceptor* (make-instance 'hunchentoot:easy-acceptor :port 8282))
(setf (hunchentoot:acceptor-document-root *acceptor*) #p".")

;;
;; "main" function used by buildapp
;;
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

;;
;; Starts web server. Called by "main", or directly from repl
;;
(defun start-web()
  (format *error-output* "Starting web server~%")
  (hunchentoot:start *acceptor*)
  (format *error-output*  "Web server started~%"))

;;
;; Stops web server
;;
(defun stop-web()
  (hunchentoot:stop *acceptor*))

;;
;; An endpoint that uses spinneret to generate a web page.
;;
(hunchentoot:define-easy-handler (say-hi :uri "/hi") ()
  (setf (hunchentoot:content-type*) "text/html")
  (spinneret:with-html-string
    (:doctype)
    (:html
     (:head
      (:title "Hello World"))
     (:body
      (:h1 (format nil "Hello World, from ~d" (machine-instance)))
      (:p "Hello!"))))
  )

;;
;; What follows is an attempt at parameterizing the form above
;;

;; Wraps a "title" and "body" in boilerplate html
(defmacro with-page ((&key title) &body body)
  `(quote ((:doctype)
     (:html
      (:head
       (:title ,title))
      (:body ,@body)))))

;; Just a thin wrapper around with-page, but shows how you can extend
;; with-page to contain more structure ("boilerplate") in the body.
(defmacro simple-page((&key title) &body body)
  `(with-page (:title ,title) (:h1 ,title) ,@body))

(defmacro endpoint((&key key) (&body body))
  `(hunchentoot:define-easy-handler
       (,(make-symbol (string-upcase (concatenate 'string "say-" key)))
        :uri ,(concatenate 'string "/" key)) ()
     (setf (hunchentoot:content-type*) "text/html")
     (spinneret:with-html-string ,@body)))

(defmacro html-endpoint((&key key) (&key title) &body body)
  ; I don't know how to do this without an (eval)
  `(endpoint (:key ,key) ,(eval `(simple-page (:title ,title) ,@body))))

(html-endpoint (:key "hola") (:title "Hola!") (:p "Hola Mundo"))

