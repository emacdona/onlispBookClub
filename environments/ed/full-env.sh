#!/usr/bin/env bash
set -eo pipefail

ENVIRONMENT_ROOT="$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

${ENVIRONMENT_ROOT}/env.sh -d xfce4-terminal
${ENVIRONMENT_ROOT}/env.sh -d emacs
${ENVIRONMENT_ROOT}/env.sh -d firefox
