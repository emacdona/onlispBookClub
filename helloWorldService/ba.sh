#!/usr/bin/env bash
set -eo pipefail

# Decide which of these to save and include in some docs...
# http://margaine.com/2015/04/29/common-lisp-and-docker.html
# https://www.xach.com/lisp/buildapp/
# https://www.quicklisp.org/beta/UNOFFICIAL/docs/buildapp/doc/index.html
# https://stackoverflow.com/questions/25858237/confused-about-qlquickload-and-executable-scripts-in-sbcl
# https://stackoverflow.com/questions/18917067/how-to-use-buildapp-in-combination-with-quicklisp
# http://edicl.github.io/hunchentoot/
# https://stackoverflow.com/questions/48103501/deploying-common-lisp-web-applications

sbcl \
   --non-interactive \
   --load web.lisp \
   --eval '(ql:write-asdf-manifest-file "quicklisp-manifest.txt")'

buildapp \
   --manifest-file ./quicklisp-manifest.txt \
   --load ~/quicklisp/setup.lisp \
   --load web.lisp \
   --output helloworld.bin \
   --entry main
