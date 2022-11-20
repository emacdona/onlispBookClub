# How to write better Makefiles:
# https://tech.davis-hansson.com/p/make/

# Becoming your own CA:
# https://stackoverflow.com/a/27931596
# https://stackoverflow.com/questions/21297139/how-do-you-sign-a-certificate-signing-request-with-your-certification-authority/21340898#21340898

# Using whatever certs are in k8s (however they got there) as your CA (for client certs)
# With k3d, at least, this was so much f*cking easier than becoming your own CA (for client certs). This is because even after
# you jump through all the hoops to be a CA, you STILL need to figure out how to get k8s to use that cert as its CA.
# https://kubernetes.io/docs/reference/access-authn-authz/certificate-signing-requests/#kubernetes-signers

SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:
.DEFAULT_GOAL := servercert.pem
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

projectDir = $(shell pwd)
target = "$(projectDir)/target"

clean:
	rm -f *.pem *.csr *.pfx index.txt* serial.txt*

all: clean clientcert.pfx cacert.pem servercert.pem clientcert.pem
.PHONY: all

servercert-secret: serverkey.pem servercert.pem
	#kubectl create namespace tls
	kubectl create secret tls server-cert \
		--namespace kubernetes-dashboard \
		--key serverkey.pem \
		--cert servercert.pem

# Not idempotent. If a csr for this identity already exists, it will quietly just not create a new cert and instead send
# the old one back -- and then your new key won't match it. At least that's my theory based on the behaviour I've seen.
kubeclientcert.pfx: kubeclientcert.pem clientkey.pem
	openssl \
		pkcs12 \
		-export  \
		-inkey clientkey.pem\
		-in kubeclientcert.pem \
		-passout 'pass:' \
		-out kubeclientcert.pfx

kubeclientcert.pem: clientcert.csr
	kubectl apply -f <(m4 -D __CSR__=$$(cat clientcert.csr | base64 | tr -d "\n") kubecsr.m4)
	kubectl certificate approve cluster-admin 
	kubectl get csr/cluster-admin -o json | jq -r '.status.certificate' | base64 -d > kubeclientcert.pem

inspect-ca: cacert.pem
	openssl x509 -in cacert.pem -text -noout
.PHONY: inspect-ca

view-ca-purpose: cacert.pem
	openssl x509 -purpose -in cacert.pem -inform PEM
.PHONY: view-ca-purpose

cacert.pem cakey.pem: openssl-ca.cnf
	openssl \
  		req \
  		-x509 \
  		-config openssl-ca.cnf \
  		-newkey rsa:4096 \
  		-sha256 \
  		-nodes \
  		-out cacert.pem \
  		-outform PEM

servercert.csr serverkey.pem: openssl-server.cnf
	openssl \
  		req \
  		-config openssl-server.cnf \
  		-newkey rsa:2048 \
  		-sha256 \
  		-nodes \
  		-out servercert.csr \
  		-outform PEM

inspect-server-csr: servercert.csr
	openssl req -text -noout -verify -in servercert.csr
.PHONY: inspect-server-csr

servercert.pem: servercert.csr cacert.pem cakey.pem | index.txt serial.txt
	openssl \
  		ca \
  		-batch \
  		-config openssl-ca.cnf \
  		-policy signing_policy \
  		-extensions signing_req \
  		-out servercert.pem \
  		-infiles servercert.csr

inspect-server-cert: servercert.pem
	openssl x509 -in servercert.pem -text -noout
.PHONY: inspect-server-csr

clientcert.csr clientkey.pem: openssl-client.cnf
	openssl \
  		req \
  		-config openssl-client.cnf \
  		-newkey rsa:2048 \
  		-sha256 \
  		-nodes \
  		-out clientcert.csr \
  		-outform PEM

inspect-client-csr: clientcert.csr
	openssl req -text -noout -verify -in clientcert.csr
.PHONY: inspect-client-csr

# Firefox doesn't like to import client certs in pem format, even if you include pem format cert and private key in same file.
clientcert.pfx: clientcert.pem clientkey.pem cacert.pem
	openssl \
		pkcs12 \
		-export  \
		-inkey clientkey.pem \
		-in clientcert.pem \
		-certfile cacert.pem \
		-passout 'pass:' \
		-out clientcert.pfx

clientcert.pem: clientcert.csr cacert.pem cakey.pem | index.txt serial.txt
	openssl \
  		ca \
  		-batch \
  		-config openssl-ca.cnf \
  		-policy signing_policy \
  		-extensions signing_req \
  		-out clientcert.pem \
  		-infiles clientcert.csr

inspect-client-cert: clientcert.pem
	openssl x509 -in clientcert.pem -text -noout
.PHONY: inspect-client-csr

index.txt serial.txt:
	rm -f index.txt serial.txt
	touch index.txt
	echo '01' > serial.txt
