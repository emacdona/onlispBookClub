#!/usr/bin/env bash
set -eo pipefail

# Note: Currently, this script requires the following be installed:
# docker
# It probably also will only run on Linux. Maybe with a little work, it could run on Mac. With a BIT more
# work, it could run on Windows.

usage() {
   echo "Usage: $0 [-n <git name>] [-e <git email>] [-d (starts container in daemon mode)] [-c (don't use cache for docker build)]" 1>&2;
    echo "   If arguments not specified, you need to have git installed because:" 1>&2;
    echo "      git name defaults to: git config --global user.name" 1>&2;
    echo "      git email defaults to: git config --global user.email" 1>&2;
    exit 1;
}

USER_ENTERED_GIT_NAME=""
USER_ENTERED_GIT_EMAIL=""
DAEMON=""
NO_CACHE_DOCKER_BUILD=""
TAG=""

echo "Started with arguments: $@"

while getopts ":n:e:dct:" o; do
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
        c)
            NO_CACHE_DOCKER_BUILD="--no-cache"
            ;;
        t)
            TAG=${OPTARG}
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
   FIREFOX_SETTINGS=".mozilla"

   mkdir -p "${ENVIRONMENT_ROOT}/.volumes/${JETBRAINS_PLUGINS}"
   mkdir -p "${ENVIRONMENT_ROOT}/.volumes/${JETBRAINS_CONFIGURATION}"
   mkdir -p "${ENVIRONMENT_ROOT}/.volumes/${JETBRAINS_SYSTEM}"
   mkdir -p "${ENVIRONMENT_ROOT}/.volumes/${JETBRAINS_LOGS}"
   mkdir -p "${ENVIRONMENT_ROOT}/.volumes/${JETBRAINS_PROJECTS}"
   mkdir -p "${ENVIRONMENT_ROOT}/.volumes/${FIREFOX_SETTINGS}"

   JETBRAINS_PLUGINS_VOLUME="${ENVIRONMENT_ROOT}/.volumes/${JETBRAINS_PLUGINS}:/home/${USER}/${JETBRAINS_PLUGINS}:rw"
   JETBRAINS_CONFIGURATION_VOLUME="${ENVIRONMENT_ROOT}/.volumes/${JETBRAINS_CONFIGURATION}:/home/${USER}/${JETBRAINS_CONFIGURATION}:rw"
   JETBRAINS_SYSTEM_VOLUME="${ENVIRONMENT_ROOT}/.volumes/${JETBRAINS_SYSTEM}:/home/${USER}/${JETBRAINS_SYSTEM}:rw"
   JETBRAINS_LOGS_VOLUME="${ENVIRONMENT_ROOT}/.volumes/${JETBRAINS_LOGS}:/home/${USER}/${JETBRAINS_LOGS}:rw"
   JETBRAINS_PROJECTS_VOLUME="${ENVIRONMENT_ROOT}/.volumes/${JETBRAINS_PROJECTS}:/home/${USER}/${JETBRAINS_PROJECTS}"
   FIREFOX_SETTINGS_VOLUME="${ENVIRONMENT_ROOT}/.volumes/${FIREFOX_SETTINGS}:/home/${USER}/${FIREFOX_SETTINGS}"


   echo "Git User: ${GIT_USER_NAME}"
   echo "Git Email: ${GIT_USER_EMAIL}"

   if [ -z "${TAG}" ]
   then
      cd "${ENVIRONMENT_ROOT}/docker" && \
         docker build \
            ${NO_CACHE_DOCKER_BUILD} \
            --build-arg USERNAME="${USER}" \
            --build-arg UID=$(id -u ${USER}) \
            --build-arg USERGROUP="${USER}" \
            --build-arg GID="$(id -g ${USER})" \
            --build-arg DOCKER_GID="$(cat /etc/group | grep "^docker" | awk -F ":" '{print $3}')" \
            --build-arg GIT_USER_EMAIL="${GIT_USER_EMAIL}" \
            --build-arg GIT_USER_NAME="${GIT_USER_NAME}" \
            -t emacdona/onlisp .

      TAG="emacdona/onlisp"
   fi

   # https://stackoverflow.com/questions/24225647/docker-a-way-to-give-access-to-a-host-usb-or-serial-device
   # (scroll down to the answer by "Wout_bb"). What a f*cking pain it is to get any information about usb devices...
   YUBI_DEVICE_MAJOR_NUMBER=$(ls -la $(lsusb | grep Yubico | awk '{print "/dev/bus/usb/" $2 "/" $4}'  | sed 's/:$//') | awk '{print $5}' | sed 's/,$//')

   # https://jtreminio.com/blog/running-docker-containers-as-current-host-user/
   # https://jpetazzo.github.io/2015/09/03/do-not-use-docker-in-docker-for-ci/
   # Separating this into "run" then "exec" lets you leave the container running so you can exit
   # and then reconnect to it later
   if [ -z "${DAEMON}" ]
   then
       # "host.docker.internal": https://github.com/moby/moby/pull/40007#issuecomment-578729356
       # https://robcogenteam.bitbucket.io/bonus-download.html (re: --security-opt)
       # Re: shm-size (an attempt to fix crashing vscode):
       #  https://github.com/microsoft/vscode/issues/139984
       #  https://www.cyberciti.biz/tips/what-is-devshm-and-its-practical-usage.html
       #  https://news.ycombinator.com/item?id=12578908 -- use ".test" domain; RFC-6761 approved
   CONTAINER_ID=$(docker run \
      --device-cgroup-rule="c ${YUBI_DEVICE_MAJOR_NUMBER}:* rmw" \
      -v /run/udev:/run/udev:ro \
      -v /dev:/dev \
      --shm-size 4G \
      --add-host host.docker.internal:host-gateway \
      --add-host grafana.test:host-gateway \
      --add-host kibana.test:host-gateway \
      --add-host keycloak.test:host-gateway \
      --add-host jenkins.test:host-gateway \
      --add-host helloworld.lisp.test:host-gateway \
      --security-opt seccomp=unconfined \
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
      -v "${FIREFOX_SETTINGS_VOLUME}" \
      -v "${HOME}/.ssh":"/home/${USER}/.ssh":ro \
      -v "/tmp/.X11-unix:/tmp/.X11-unix" \
      -v /var/run/docker.sock:/var/run/docker.sock \
      -e "TERM=xterm-256color" \
      -e "DISPLAY=${DISPLAY}" \
      -td "${TAG}" zsh)
   else
   echo "Running in daemon mode"
   set -x
   docker run --rm -d \
      --device-cgroup-rule="c ${YUBI_DEVICE_MAJOR_NUMBER}:* rmw" \
      -v /run/udev:/run/udev:ro \
      -v /dev:/dev \
      --shm-size 4G \
      --add-host host.docker.internal:host-gateway \
      --add-host grafana.test:host-gateway \
      --add-host kibana.test:host-gateway \
      --add-host keycloak.test:host-gateway \
      --add-host jenkins.test:host-gateway \
      --add-host helloworld.lisp.test:host-gateway \
      --security-opt seccomp=unconfined \
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
      -v "${FIREFOX_SETTINGS_VOLUME}" \
      -v "${HOME}/.ssh":"/home/${USER}/.ssh":ro \
      -v "/tmp/.X11-unix:/tmp/.X11-unix" \
      -v /var/run/docker.sock:/var/run/docker.sock \
      -e "TERM=xterm-256color" \
      -e "DISPLAY=${DISPLAY}" \
      -td "${TAG}" "$@"
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

