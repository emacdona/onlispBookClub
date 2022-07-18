# abcl-playground
### _Your Name <your.name@example.com>_

This is a project to do ... something.

## License

Specify license here

## How this directory was initialized
## (Don't forget the trailing "/" in the directory name!)
(ql:quickload "quickproject")
(quickproject:make-project 
    "/home/emacdona/onlisp/interestingExamples/abcl/abcl-playground/")

## How to add this to your system load path.
## ql:quickload does this for you the first time
(setf asdf:*central-registry* 
    (append asdf:*central-registry* 
    '("/home/emacdona/onlisp/interestingExamples/abcl/abcl-playground/")))

(asdf:load-system 'abcl-playground)

Notes:
ABCL (via :java-collections) implements SEQUENCE protocols for java collections. Part of the definition of "sequence" is a fixed ordering, which means the expectation is that sequences can be indexed. This excludes Java Sets.

A Cons is a Sequence, but a Sequence is not necessarily a Cons. IE: You can't call (car ) on all sequences.

I couldn't understand the difference between:
(asdf:load-system 'abcl-playground)
and
(require 'abcl-playground)

It turns out, there isn't one (for some implementations):
https://asdf.common-lisp.dev/asdf/Loading-a-system.html
"On some implementations (see Convenience Functions), ASDF hooks into the cl:require facility"
