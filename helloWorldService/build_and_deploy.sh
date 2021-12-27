#!/usr/bin/env bash
set -eo pipefail


function dockerRepoPort(){
   docker \
      ps \
      -f name=registry \
      --format "{{json .}}" \
   | jq -r '.Ports' \
   | perl -ne 'print((split(/:/, (split(/->/))[0]))[1], "\n")'
}

docker build . -t net.edmacdonald/lisphelloworld:latest

docker tag net.edmacdonald/lisphelloworld:latest localhost:$(dockerRepoPort)/net.edmacdonald/lisphelloworld:latest

docker push localhost:$(dockerRepoPort)/net.edmacdonald/lisphelloworld:latest
