# https://tech.davis-hansson.com/p/make/

SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

projectDir = $(shell pwd)


dockerRegistryPort=\
docker ps \
-f name=registry \
--format "{{json .}}" \
| jq -r '.Ports' \
| perl -ne 'print((split(/:/, (split(/->/))[0]))[1], "\n")'

#test:
#	echo $(shell $(dockerRegistryPort))

provision-cluster: create-cluster build-containers
	. ~/venv/ansible/bin/activate
	ansible-playbook -vv --extra-vars "k3d_registry_port=$(shell $(dockerRegistryPort))" --inventory inventory/inventory.yaml cluster.yml
.PHONY: provision-cluster

build-containers: build-keycloak
.PHONY: pre-provision

build-keycloak:
	cd containers/keycloak && make docker-push
.PHONY: build-keycloak

create-cluster: delete-cluster
# If/when we decide to replace k8s certs with ones we generate ourself, start here. But there's a lot
# more work to do: https://kubernetes.io/docs/setup/best-practices/certificates/
# ALL I WANT is to specify another cert that is a CA for identifying clients, FFS. But, if I try to add just
# 'client-ca-file', it replaces the existing one and brakes ALL intra cluster communication. I cant find a way to "add"
# a CA... just "replace". I think the solution is to generate ALL certs... but when do do that, I have no idea (it
# would need to be early in the process so that k3d could use those certs to generate client certs within the cluster)
#	cd certs && make all
#		--volume "$(shell pwd)/certs:/my-certs@server:0" \
#		--k3s-arg "--kube-apiserver-arg=client-ca-file=/my-certs/cacert.pem@server:0"
	k3d cluster create \
		--registry-create registry \
		--api-port 172.17.0.1:42042 \
		-p "80:80@loadbalancer" \
		-p "443:443@loadbalancer" \
		-p "8080:8080@loadbalancer" \
		-p "8443:8443@loadbalancer" \
		-p "32022:32022@server:0" \
		--k3s-arg '--no-deploy=traefik@server:0'
.PHONY: create-cluster

clean: delete-cluster clean-subprojects
.PHONY: clean

delete-cluster:
	k3d cluster delete
.PHONY: delete-cluster

clean-subprojects:
	cd containers/keycloak && make clean
.PHONY: clean-subprojects
