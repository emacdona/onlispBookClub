;;;; abcl-playground.lisp

(in-package #:abcl-playground)
(require :java-collections)
(require :jss)

(defun main ()
  (pprint (#"getProviders" 'java.security.Security))

  (#"addProvider"
   'java.security.Security
   (jss:new 'org.bouncycastle.jce.provider.BouncyCastleProvider))

  (pprint
   (#"getName"
    (#"getProvider"
     (#"getInstance" 'java.security.MessageDigest "SHA1"))))

  (pprint
   (#"getName"
    (#"getProvider"
     (#"getInstance" 'java.security.MessageDigest "SHA1" "BC")))))

;; I CAN call (car) on what this returns (is a cons?)
(defun set2list (set)
  (map 'list
       (lambda (x) x)
       ;; I can't call (car) on this, even though I can call (map) on it
       ;; (not a cons?)
       (jss:new 'java.util.arraylist set)))

(defun provider-meta ()
  (let ((providerMeta (#"getProvider" 'java.security.Security "BC")))
    (map 'list
         (lambda (x) (format nil "~a => ~a" x (#"get" providerMeta x)))
         (set2list (#"keySet" providerMeta)))))
