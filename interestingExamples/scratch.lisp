
;;
;; What if I want to write a macro that provides clients the ability to generate some
;; of the code to be executed?
;;
;; I presume I would do so by allowing callers to provide my macro a function that I will
;; call and then embed its results in the code that I generate.
;;
;; What do I request that they pass me a function and not a macro? Because I wish no
;; to force upon them the restriction that they name what they pass me, and I know of
;; no way to create an unnamed macro. "lambda" is how to do so for functions.
;;
;; The fact that I require the range of their function be expressions makes it VERY much
;; like a macro, however...


(defmacro compile-time-execution-of-dynamically-generated-code-e1(command-generator)
  `(progn
     ,@(funcall command-generator)))

(defun execution-1 ()
  (compile-time-execution-of-dynamically-generated-code-e1
   (lambda () '((format t "Hello World")))))

(defun expansion-1 ()
  (macroexpand-1
   '(compile-time-execution-of-dynamically-generated-code-e1
     (lambda () '((format t "Hello World"))))))

;;
;; This works... but what environment is 'command-generator' evaluated in?

(defmacro compile-time-execution-of-dynamically-generated-code(command-generator)
  `(progn
     ,@(funcall (eval command-generator))))

(defun execution-2 ()
  (compile-time-execution-of-dynamically-generated-code
   (lambda () '((format t "Hello World")))))

(defun expansion-2 ()
  (macroexpand-1
   '(compile-time-execution-of-dynamically-generated-code-e1
     (lambda () '((format t "Hello World"))))))


;; This is a compile error. Is it because...
;; compile-time-execution-of-dynamically-generated-code is expanded at compile time and
;; gen doesn't have a binding until run time?
;; (let ((greeting "Hello World"))
;;   (flet ((gen () '((format t greeting))))
;;     (lambda ()
;;       (compile-time-execution-of-dynamically-generated-code gen))
;;     ))



(defmacro code-generator-caller-1 (code-generator)
  `(quote ,(funcall code-generator)))

(defmacro code-generator-caller-2 (code-generator)
  `(quote ,(funcall (eval code-generator))))

(defmacro run-example (symbol)
  `(,symbol (lambda () 'hello)))

;; (run-example funcall)
;; yields:
;; HELLO
;;
;; (run code-generator-caller-2)
;; yields:
;; HELLO
;;
;; (run code-generator-caller-1)
;; yields:
;; The value
;;   (LAMBDA () 'HELLO)
;; is not of type
;;   (OR FUNCTION SYMBOL)
;;    [Condition of type TYPE-ERROR]


(macrolet ((execute-lambda-at-compile-time-a (l)
             `(quote
               ,(handler-case (funcall l)
                  (error (e) (format nil "~a" e)))))

           (execute-lambda-at-compile-time-b (l)
             `(quote
               ,(handler-case (funcall (eval l))
                  (error (e) (format nil "~a" e)))))

           (run-example (symbol)
             `(,symbol (lambda () 'hello))))
  (list
   (run-example funcall)
   (run-example execute-lambda-at-compile-time-b)
   (run-example execute-lambda-at-compile-time-a)))

