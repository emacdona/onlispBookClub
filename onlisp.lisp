
;; On Lisp


;; Chapters 1, 2
;;

;; Common Lisp is a "Lisp 2" -- meaning that each symbol has a slot to store a value
;; and a separate slot to store a function (hence the following two definitions do NOT
;; conflict)
;;
;; Note: defvar will not redefine a variable if it already has a value. This may suprise you
;; if you change it here and reload the whole file. Probably the quickest way around it is:
;; M-x slime-restart-inferior-lisp
;; followed by:
;; C-c C-l <=> M-x slime-load-file
;; though you can one off with:
;; C-M-x <=> M-x slime-eval-defun
(defun increment(x)
  (+ x 1))

(defvar increment
  '(1 2 3 4 5 6 7 8 9 10))

(defun inc-list (f l)
  (mapcar f l))

;; When you want the FUNCTION named by a symbol, you must "sharp quote" it, ie: #'
;; By default, you will get the VALUE named by a symbol
(defun example-inc-list()
  (inc-list #'increment increment))


(defun people-finder()
    (let ((people '(((fname . "ed") (lname . "macdonald"))
                    ((fname . "jane") (lname . "doe"))
                    ((fname . "john") (lname . "doe"))
                    ((fname . "ed") (lname . "norton")))))
      (list
       (lambda (firstName)
         (remove-if-not
          (lambda (person)
            (equalp firstName (cdr (assoc 'fname person))))
          people))
       (lambda (lastName)
         (remove-if-not
          (lambda (person)
            (equalp lastName (cdr (assoc 'lname person))))
          people))
       (lambda ()
         people)
       )))

(defvar finders (people-finder))

(defun find-by-first-name(firstName)
  (funcall (car finders) firstName))

(defun find-by-last-name(lastName)
  (funcall (cadr finders) lastName))

(defun list-all()
  (funcall (caddr finders)))

;; Trying to show how conses are built. a starts as a cons of 0 and nil (a one element list).
;; b and c are two item lists, each conses of a value (1 and 2 respectively) and the list a.
;; This example shows that mutating the car of a mutates the car of the cdr of both b and c.
(defun cons-exmaple ()
  (let ((a (cons 2 (cons 1 (cons 0 nil)))))
    (let ((b (cons 100 a))
          (c (cons 200 a)))
      (setf (car a) 10)
      (list a b c))))

(defun my-reverse-list (lst)
  (labels ((rl (lst acc)
             (if (null lst)
                 acc
                 (rl (cdr lst)
                     (cons (car lst) acc)))))
    (rl lst '())))

;; Unlike in the cons-example, the list returned from this function is built
;; of entirely new conses, so messing with elements of one does not affect
;; the other.
;; I think what I'm also showing here is that primitive values (eg: numbers)
;; are passed by value and not by reference.
(defun list-example ()
    (let ((a '(1 2 3 4 5 6 7 8 9 10)))
      (let ((b (my-reverse-list a)))
        (setf (car a) 100)
        (setf (car (cdr a)) 200)
        (list a b)
        )))



