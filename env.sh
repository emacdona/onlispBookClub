#!/usr/bin/env bash
set -eo pipefail

# TODO: use getopts to capture these -- use git values for default
GIT_USER_EMAIL=$(git config --global user.email)
GIT_USER_NAME=$(git config --global user.name)

PROJECT_ROOT="$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

cd "${PROJECT_ROOT}/docker" && \
   docker build \
      --build-arg USERNAME="${USER}" \
      --build-arg UID=$(id -u ${USER}) \
      --build-arg USERGROUP="${USER}" \
      --build-arg GID="$(id -g ${USER})" \
      --build-arg GIT_USER_EMAIL="${GIT_USER_EMAIL}" \
      --build-arg GIT_USER_NAME="${GIT_USER_NAME}" \
      -t emacdona/onlisp .

# https://jtreminio.com/blog/running-docker-containers-as-current-host-user/
docker run \
   -v "${PROJECT_ROOT}":"/home/${USER}/onlisp" \
   -v "${PROJECT_ROOT}/docker/.exrc":"/home/${USER}/.exrc" \
   -v "${PROJECT_ROOT}/docker/.screenrc":"/home/${USER}/.screenrc" \
   -v "${PROJECT_ROOT}/docker/.zshrc":"/home/${USER}/.zshrc" \
   -v "${PROJECT_ROOT}/docker/.emacs":"/home/emacdona/.emacs" \
   -v "${HOME}/.ssh":"/home/${USER}/.ssh":ro \
   -it emacdona/onlisp $@

