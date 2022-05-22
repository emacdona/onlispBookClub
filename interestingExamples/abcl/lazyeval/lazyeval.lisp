;;;; lazyeval.lisp

(in-package #:lazyeval)

(defmacro defunle (name args &body body)
  `(defun ,name ,args
     (let ((values (make-hash-table)))
           (flet (,@(map 'list
                  (lambda (sym)
                    `(,sym ()
                           (multiple-value-bind (value present) (gethash ',sym values)
                             (if present
                                 value
                                 (let ((value ,sym))
                                   (setf (gethash ',sym values) value)
                                   value)))))
                      args))
             ,@(labels
                 ((subsyms (syms tree)
                    (if syms
                        (let ((s (car syms)))
                          (subsyms (cdr syms) (subst `(,s) s tree)))
                        tree)))
               (subsyms args body))))))

