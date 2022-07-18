;; next three lines taken from here: https://github.com/armedbear/abcl
(require :asdf)
(require :abcl-contrib)
(asdf:load-system :quicklisp-abcl)
(ql:quickload :cl-ppcre)

(defun void-function (param)
  (jstatic "parseInt" "java.lang.Integer" param))


(defun println (param)
  (jcall
   (jmethod (jclass "java.io.PrintStream") "println" (jclass "java.lang.String"))
   (jfield "java.lang.System" "out")
   param)
  param)

(defun my-classpath ()
  (let ((*standard-output* (make-string-output-stream)))
    (uiop:run-program '("./gradlew" ":app:runtimeClasspath" "--quiet") :output t)
    (cl-ppcre:split
     ":"
     (string-trim
      '(#\Space #\Tab #\Newline)
      (get-output-stream-string *standard-output*)))))

(defmacro with-java-context ((&key classspecs methodspecs staticmethodspecs fieldspecs)
                             &body body)
  `(let*
       ,(append
         (loop for classspec in classspecs
               collect `(,(car classspec) (jclass ,(cadr classspec))))
         (loop for methodspec in methodspecs
               collect `(,(car methodspec) (jmethod ,@(cdr methodspec))))
         (loop for fieldspec in fieldspecs
               collect `(,(car fieldspec) (jfield ,@(cdr fieldspec))))
         )
     (macrolet
         ,(append
           (loop for methodspec in methodspecs
                 collect `(,(car methodspec) (&rest rest)
                           (append
                            (list
                             'jcall
                             ',(car methodspec))
                            rest)))
           (loop for staticmethodspec in staticmethodspecs
                 collect `(,(car staticmethodspec) (&rest rest)
                           (append
                            (list
                             'jstatic
                             ',(caddr staticmethodspec)
                             ',(cadr staticmethodspec))
                            rest))))
       ,@body)))

(defmacro with-java-context-param (param &body body)
  `(with-java-context ,(eval param) ,@body))

(defparameter *standard-java-context*
  `(:classspecs
   ((string "java.lang.String")
    (object "java.lang.Object")
    (integer "java.lang.Integer")
    (print-stream "java.io.PrintStream")
    (system "java.lang.System"))
   :methodspecs
   ((println print-stream "println" object))
   :staticmethodspecs
   ((parse-int integer "parseInt" string))
   :fieldspecs
   ((out system "out"))))

(defparameter *bc-context*
  `(:classspecs
    ((string "java.lang.String")
     (object "java.lang.Object")
     (integer "java.lang.Integer")
     (print-stream "java.io.PrintStream")
     (system "java.lang.System")
     (security "java.security.Security")
     (bouncy-castle-provider "org.bouncycastle.jce.provider.BouncyCastleProvider"))
    :methodspecs
    ((println print-stream "println" object))
    :staticmethodspecs
    ((parse-int integer "parseInt" string)
     (security-add-provider "addProvider" bouncy-castle-provider))
    :fieldspecs
    ((out system "out"))))


;;(pprint (macroexpand (quote
(with-java-context-param *standard-java-context*
  (println out "Hello World")
  (println out (parse-int "101")))
;;)))

(with-java-context-param *bc-context*
  (println out "Hello World")
  (println out (parse-int "101")))

;;(pprint (macroexpand (quote
(with-java-context
    (:classspecs
     ((string "java.lang.String")
      (object "java.lang.Object")
      (integer "java.lang.Integer")
      (print-stream "java.io.PrintStream")
      (system "java.lang.System"))
     :methodspecs
     ((println print-stream "println" object))
     :staticmethodspecs
     ((parse-int integer "parseInt" string))
     :fieldspecs
     ((out system "out")))
  (println out "Hello World")
  (println out (parse-int "10")))
;;)))

