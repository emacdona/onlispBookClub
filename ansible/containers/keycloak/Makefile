# https://tech.davis-hansson.com/p/make/

SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

projectDir = $(shell pwd)
keycloakTag=kc-16-b4
applicationArtifact=$(projectDir)/scim-for-keycloak/scim-for-keycloak-deployment/target/scim-for-keycloak-$(keycloakTag).ear

dockerRepoPort = $(shell docker ps -f name=registry --format "{{json .}}" \
	| jq -r '.Ports' \
	| perl -ne 'print((split(/:/, (split(/->/))[0]))[1], "\n")')

# Push the image to the
docker-push: docker-build
	echo "Docker Repo Port: $(dockerRepoPort)"
	docker tag net.edmacdonald/customkeycloak:latest localhost:$(dockerRepoPort)/net.edmacdonald/customkeycloak:latest
	docker push localhost:$(dockerRepoPort)/net.edmacdonald/customkeycloak:latest
.PHONY: docker-push

docker-build: $(applicationArtifact)
	cd $(projectDir)/docker
	cp $(applicationArtifact) .
	docker build . -t net.edmacdonald/customkeycloak:latest
.PHONY: docker-build

$(applicationArtifact):
	rm -rf scim-for-keycloak
	git clone https://github.com/Captain-P-Goldfish/scim-for-keycloak.git
	cd scim-for-keycloak
	git checkout $(keycloakTag)
	mvn clean package -DskipTests
