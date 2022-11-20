(defparameter level -1
  "Level of nesting in JSON structure.")

(defparameter count -1
  "Count of members in JSON structure.")

(defun format-with-indent (control &rest args)
  (format t "~&~0,1,V,V@A~?" level #\Tab "" control args))

(defun report-atom (token)
  (format-with-indent "~Ctoken: ~S~%" #\Tab token)
  'atom)

(defun report-push ()
  (incf level)
  (setq count 0)
  (format-with-indent "beginning of aggregate~%"))

(defun report-member (member)
  (format-with-indent "~:R member: ~A~%"
                      (incf count) member))

(defun report-pop ()
  (format-with-indent "end of aggregate~%")
  (list 'aggregate count))

(let ((foo
        (json:bind-custom-vars
            (:integer              #'report-atom
             :real                 #'report-atom
             :boolean              #'report-atom
             :beginning-of-array   #'report-push
             :array-member         #'report-member
             :end-of-array         #'report-pop
             :beginning-of-string  #'report-push
             :string-char          #'report-atom
             :end-of-string        #'report-pop
             :beginning-of-object  #'report-push
             :object-key           (constantly t)
             :object-value         #'report-member
             :end-of-object        #'report-pop
             :aggregate-scope      (list 'level 'count))
          (json:decode-json-from-string
           "{\"foo\": [10, 20, 50], \"bar\": true}"))))
  (format t "~%foo:~%")
  (describe foo))
