
(defmacro abbrev (short long)
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))


(defmacro abbrev (short long)
  (list (quote defmacro)
        short
        (quote (&rest args))
        (list (quote cons)
              (list (quote quote)
                    long)
              (quote args))))

(let ((short-name 'a) (long-name 'aye))
  (defmacro short-name (&rest args))
  `(,long-name (&rest args)))
