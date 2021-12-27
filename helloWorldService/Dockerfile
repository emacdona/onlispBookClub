FROM ubuntu:20.04

RUN apt-get update

RUN DEBIAN_FRONTEND=noninteractive apt-get install -y sbcl curl buildapp

# Quicklsip
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp
COPY quicklisp-install-script.lisp quicklisp-install-script.lisp
RUN echo | sbcl --load quicklisp.lisp --script quicklisp-install-script.lisp

COPY web.lisp web.lisp
COPY ba.sh ba.sh
RUN ./ba.sh

# Load it to get all the quicklisp libraries in the container (ie: not fetched at container startup)
# RUN echo | sbcl --load web.lisp

EXPOSE 8282

# ENTRYPOINT ["sbcl", "--load", "web.lisp", "--eval", "(start-web)"]
ENTRYPOINT ["./helloworld.bin"]
