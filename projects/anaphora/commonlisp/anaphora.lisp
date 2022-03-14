(defpackage :anaphora (:use :cl))

;; because (lambda (it)) is too much to type?
(defmacro map-with-it (list &body body)
  `(map 'list (lambda (it) ,@body) ',list))

;; because maybe people expect the first argument to be evaluated?
(defmacro bad-emap-with-it (list &body body)
  ;; Why 'let', and not just evaluate inline? Because if this macro ever gets more complex, we'd
  ;; like to ensure that list still only ever gets evaluated once
  `(let ((l ,list))
     (map 'list (lambda (it) ,@body) l)))

(defmacro emap-with-it (list &body body)
  (let ((l (gensym)))
  `(let ((,l ,list))
     (map 'list (lambda (it) ,@body) ,l))))

(defun test-bad-emap ()
(let ((l 10))
  (bad-emap-with-it (list 1 2 3) (* it l))))

(defun test-emap ()
  (let ((l 10))
    (emap-with-it (list 1 2 3) (* it l))))


;; eg: (map-with-it (1 2 3 4 5) (* it 10))
;; eg: (emap-with-it (list 1 2 3 4 5) (* it 10))


