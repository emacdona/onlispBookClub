;;;; abcl-playground.asd

(asdf:defsystem :abcl-playground/bcpkix :defsystem-depends-on (abcl-asdf)
  :components ((:mvn "org.bouncycastle/bcpkix-jdk18on" :version "1.71")))

(asdf:defsystem :abcl-playground/bcutil :defsystem-depends-on (abcl-asdf)
  :components ((:mvn "org.bouncycastle/bcutil-jdk18on" :version "1.71")))

(asdf:defsystem #:abcl-playground
  :description "An attempt to organize a Lisp project"
  :author "Ed MacDonald <edmacdonald555@gmail.com>"
  :license  "BSD"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "abcl-playground"))
  :depends-on (:abcl-playground/bcpkix
               :abcl-playground/bcutil
               :jss))
