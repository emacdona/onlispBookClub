(defmacro mypartial (f &rest args)
  (labels ((process-args (args)
             (if args
                 (let ((first (car args))
                       (parameter (gensym)))
                   (multiple-value-bind
                         (rest-parameters rest-arguments)
                       (process-args (cdr args))
                     (if (eq first '_)
                         (values (cons parameter rest-parameters)
                                 (cons parameter rest-arguments))
                         (values rest-parameters
                                 (cons first rest-arguments)))))
                 args)))
    (multiple-value-bind (new-function-parameters all-function-arguments)
        (process-args args)
      `(lambda (,@new-function-parameters) (,f ,@all-function-arguments)))))

(defun y (m x b)
  (+ (* m x) b))

(defun slope-intercept-line (slope intercept)
  (mypartial y slope _ intercept))

(setf (symbol-function 'y1) (slope-intercept-line 2 0))
(setf (symbol-function 'y2) (slope-intercept-line 2 -1))

(let ((indexes '(1 2 3 4 5 6 7 8)))
  (list
   (mapcar #'y1 indexes)
   (mapcar #'y2 indexes)))
