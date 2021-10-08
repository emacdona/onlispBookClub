#!/usr/bin/env bash
set -eo pipefail

./environments/ed/env.sh ./helloWorld-clisp.lisp | grep "Hello, World"
./environments/ed/env.sh ./helloWorld-sbcl.lisp | grep "Hello, World"
