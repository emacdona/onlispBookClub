#!/usr/bin/env bash
set -eo pipefail

PROJECT_ROOT="$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

cd "${PROJECT_ROOT}/docker" && \
   docker build \
      --build-arg USERNAME="${USER}" \
      --build-arg UID=$(id -u ${USER}) \
      --build-arg USERGROUP="${USER}" \
      --build-arg GID="$(id -g ${USER})" \
      -t emacdona/onlisp .

# https://jtreminio.com/blog/running-docker-containers-as-current-host-user/
docker run \
   -v "${PROJECT_ROOT}":"/home/${USER}/onlisp" \
   -v "${PROJECT_ROOT}/docker/.emacs":"/home/emacdona/.emacs" \
   -v "${PROJECT_ROOT}/docker/.exrc":"/home/emacdona/.exrc" \
   -v "${PROJECT_ROOT}/docker/.screenrc":"/home/emacdona/.screenrc" \
   -v "${PROJECT_ROOT}/docker/.zshrc":"/home/emacdona/.zshrc" \
   -it emacdona/onlisp $@
