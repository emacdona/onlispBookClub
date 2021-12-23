#!/usr/bin/env bash
set -eo pipefail

# Note: Currently, this script requires the following be installed:
# docker
# It probably also will only run on Linux. Maybe with a little work, it could run on Mac. With a BIT more
# work, it could run on Windows. 

usage() {
   echo "Usage: $0 [-n <git name>] [-e <git email>] [-d (starts container in daemon mode)]" 1>&2;
    echo "   If arguments not specified, you need to have git installed because:" 1>&2;
    echo "      git name defaults to: git config --global user.name" 1>&2;
    echo "      git email defaults to: git config --global user.email" 1>&2;
    exit 1;
}

USER_ENTERED_GIT_NAME=""
USER_ENTERED_GIT_EMAIL=""
DAEMON=""

echo "Started with arguments: $@"

while getopts ":n:e:d" o; do
    case "${o}" in
        n)
            USER_ENTERED_GIT_NAME=${OPTARG}
            ;;
        e)
            USER_ENTERED_GIT_EMAIL=${OPTARG}
            ;;
        d)
            DAEMON="y"
            ;;
        *)
            usage
            ;;
    esac
done
shift $((OPTIND-1))

echo "Ended with arguments: $@"

# Hacky, but since we already require docker, use it to run perl!
CONTAINER_ID=$(\
  docker ps | \
    docker run --rm -i perl:5.34.0 \
    perl -ne 'my($id, $image) = (split(/\s+/, $_, 3))[0,1]; print $id if $image eq "emacdona/onlisp"' \
  | head -n 1
)

if [ -z "${CONTAINER_ID}" ] || [ ! -z "${DAEMON}" ]
then
   if [ -z "${USER_ENTERED_GIT_NAME}" ]
   then
       GIT_USER_EMAIL=$(git config --global user.email)
    else
       GIT_USER_EMAIL=${USER_ENTERED_GIT_NAME}
   fi;
   if [ -z "${USER_ENTERED_GIT_EMAIL}" ]
   then
       GIT_USER_NAME=$(git config --global user.name)
    else
       GIT_USER_NAME=${USER_ENTERED_GIT_EMAIL}
   fi;

   ENVIRONMENT_ROOT="$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
   PROJECT_ROOT="${ENVIRONMENT_ROOT}/../.."

   JETBRAINS_PLUGINS=".local/share/JetBrains"
   JETBRAINS_CONFIGURATION=".config/JetBrains/IntelliJIdea2021.1"
   JETBRAINS_SYSTEM=".cache/JetBrains/IntelliJIdea2021.1"
   JETBRAINS_LOGS=".cache/JetBrains/IntelliJIdea2021.1/log"
   JETBRAINS_PROJECTS="IdeaProjects"

   mkdir -p "${ENVIRONMENT_ROOT}/.volumes/${JETBRAINS_PLUGINS}"
   mkdir -p "${ENVIRONMENT_ROOT}/.volumes/${JETBRAINS_CONFIGURATION}"
   mkdir -p "${ENVIRONMENT_ROOT}/.volumes/${JETBRAINS_SYSTEM}"
   mkdir -p "${ENVIRONMENT_ROOT}/.volumes/${JETBRAINS_LOGS}"
   mkdir -p "${ENVIRONMENT_ROOT}/.volumes/${JETBRAINS_PROJECTS}"

   JETBRAINS_PLUGINS_VOLUME="${ENVIRONMENT_ROOT}/.volumes/${JETBRAINS_PLUGINS}:/home/${USER}/${JETBRAINS_PLUGINS}:rw"
   JETBRAINS_CONFIGURATION_VOLUME="${ENVIRONMENT_ROOT}/.volumes/${JETBRAINS_CONFIGURATION}:/home/${USER}/${JETBRAINS_CONFIGURATION}:rw"
   JETBRAINS_SYSTEM_VOLUME="${ENVIRONMENT_ROOT}/.volumes/${JETBRAINS_SYSTEM}:/home/${USER}/${JETBRAINS_SYSTEM}:rw"
   JETBRAINS_LOGS_VOLUME="${ENVIRONMENT_ROOT}/.volumes/${JETBRAINS_LOGS}:/home/${USER}/${JETBRAINS_LOGS}:rw"
   JETBRAINS_PROJECTS_VOLUME="${ENVIRONMENT_ROOT}/.volumes/${JETBRAINS_PROJECTS}:/home/${USER}/${JETBRAINS_PROJECTS}"


   echo "Git User: ${GIT_USER_NAME}"
   echo "Git Email: ${GIT_USER_EMAIL}"

   cd "${ENVIRONMENT_ROOT}/docker" && \
      docker build \
         --build-arg USERNAME="${USER}" \
         --build-arg UID=$(id -u ${USER}) \
         --build-arg USERGROUP="${USER}" \
         --build-arg GID="$(id -g ${USER})" \
         --build-arg DOCKER_GID="$(cat /etc/group | grep "^docker" | awk -F ":" '{print $3}')" \
         --build-arg GIT_USER_EMAIL="${GIT_USER_EMAIL}" \
         --build-arg GIT_USER_NAME="${GIT_USER_NAME}" \
         -t emacdona/onlisp .

   # https://jtreminio.com/blog/running-docker-containers-as-current-host-user/
   # https://jpetazzo.github.io/2015/09/03/do-not-use-docker-in-docker-for-ci/
   # Separating this into "run" then "exec" lets you leave the container running so you can exit
   # and then reconnect to it later
   if [ -z "${DAEMON}" ]
   then
       # "host.docker.internal": https://github.com/moby/moby/pull/40007#issuecomment-578729356
   CONTAINER_ID=$(docker run \
      --add-host host.docker.internal:host-gateway \
      -v "${PROJECT_ROOT}":"/home/${USER}/onlisp" \
      -v "${ENVIRONMENT_ROOT}/docker/.exrc":"/home/${USER}/.exrc" \
      -v "${ENVIRONMENT_ROOT}/docker/.screenrc":"/home/${USER}/.screenrc" \
      -v "${ENVIRONMENT_ROOT}/docker/.zshrc":"/home/${USER}/.zshrc" \
      -v "${ENVIRONMENT_ROOT}/docker/.spacemacs":"/home/${USER}/.spacemacs" \
      -v "${JETBRAINS_PLUGINS_VOLUME}" \
      -v "${JETBRAINS_CONFIGURATION_VOLUME}" \
      -v "${JETBRAINS_SYSTEM_VOLUME}" \
      -v "${JETBRAINS_LOGS_VOLUME}" \
      -v "${JETBRAINS_PROJECTS_VOLUME}" \
      -v "${HOME}/.ssh":"/home/${USER}/.ssh":ro \
      -v "/tmp/.X11-unix:/tmp/.X11-unix" \
      -v /var/run/docker.sock:/var/run/docker.sock \
      -e "TERM=xterm-256color" \
      -e "DISPLAY=${DISPLAY}" \
      -td emacdona/onlisp zsh)
   else
   echo "Running in daemon mode"
   set -x
   docker run --rm -d \
      --add-host host.docker.internal:host-gateway \
      -v "${PROJECT_ROOT}":"/home/${USER}/onlisp" \
      -v "${ENVIRONMENT_ROOT}/docker/.exrc":"/home/${USER}/.exrc" \
      -v "${ENVIRONMENT_ROOT}/docker/.screenrc":"/home/${USER}/.screenrc" \
      -v "${ENVIRONMENT_ROOT}/docker/.zshrc":"/home/${USER}/.zshrc" \
      -v "${ENVIRONMENT_ROOT}/docker/.spacemacs":"/home/${USER}/.spacemacs" \
      -v "${JETBRAINS_PLUGINS_VOLUME}" \
      -v "${JETBRAINS_CONFIGURATION_VOLUME}" \
      -v "${JETBRAINS_SYSTEM_VOLUME}" \
      -v "${JETBRAINS_LOGS_VOLUME}" \
      -v "${JETBRAINS_PROJECTS_VOLUME}" \
      -v "${HOME}/.ssh":"/home/${USER}/.ssh":ro \
      -v "/tmp/.X11-unix:/tmp/.X11-unix" \
      -v /var/run/docker.sock:/var/run/docker.sock \
      -e "TERM=xterm-256color" \
      -e "DISPLAY=${DISPLAY}" \
      -td emacdona/onlisp "$@"
   set +x
   fi;
fi;

if [ -z "${DAEMON}" ]
then
   echo "Connecting to ContainerID: ${CONTAINER_ID}"

   if [ -z "$@" ]
   then
      docker exec -it $CONTAINER_ID zsh
   else
      docker exec -it $CONTAINER_ID $@
   fi;
fi;

