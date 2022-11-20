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

helm \
    install \
    --set image.repository="registry:$(dockerRepoPort)/net.edmacdonald/lisphelloworld" \
    --set replicaCount=10 \
    hw \
    ./helloworld
