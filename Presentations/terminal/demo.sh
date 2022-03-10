#!/usr/bin/env bash
set -eo pipefail

. demo-magic/demo-magic.sh

clear
pe 'docker build \
--build-arg UID=$(id -u ${USER}) \
--build-arg GID=$(id -g ${USER}) \
--build-arg USERNAME="${USER}" \
--build-arg USERGROUP="${USER}" \
-t emacdona/dockerdemo:latest \
./docker'

wait
clear
pe 'docker run -it emacdona/dockerdemo'

#wait
#clear
#pe 'docker run -it \
#-e "DISPLAY=${DISPLAY}" \
#-v /tmp/.X11-unix:/tmp/.X11-unix:ro \
#emacdona/dockerdemo \
#octave --gui'

wait
clear
pe 'docker run -it \
-e "DISPLAY=${DISPLAY}" \
-v /tmp/.X11-unix:/tmp/.X11-unix:ro \
-v `pwd`:`pwd` \
-w `pwd` \
emacdona/dockerdemo \
octave --gui'

wait
clear
pe 'cat ./plot.m'
pe 'cat ./octave.sh'

wait
clear
pe './plot.m'
