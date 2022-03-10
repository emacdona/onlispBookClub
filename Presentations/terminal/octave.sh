#!/usr/bin/env bash
set -eo pipefail


docker run --rm \
   -e "DISPLAY=${DISPLAY}" \
   -v /tmp/.X11-unix:/tmp/.X11-unix:ro \
   -v `pwd`:`pwd` \
   -w `pwd` \
   emacdona/dockerdemo \
   octave "$@"
