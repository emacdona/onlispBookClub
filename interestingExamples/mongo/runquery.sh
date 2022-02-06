#!/usr/bin/env bash
set -eo pipefail

MONGOPW="$(kubectl get secret --namespace default my-release-mongodb -o jsonpath="{.data.mongodb-root-password}" | base64 --decode)"
mongosh --eval "var mongopw='${MONGOPW}'" --file query.js
