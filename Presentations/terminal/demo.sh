#!/usr/bin/env bash
set -eo pipefail

. demo-magic/demo-magic.sh

clear
wait
printf "# Build the container.\n\n"
pe 'docker build \
--build-arg UID=$(id -u ${USER}) \
--build-arg GID=$(id -g ${USER}) \
--build-arg USERNAME="${USER}" \
--build-arg USERGROUP="${USER}" \
-t emacdona/dockerdemo:latest \
./docker'

wait
clear
printf "# Run the container.\n"
printf "# Use its default CMD.\n\n"
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
printf "# Run the container.\n"
printf "# Bind mount XWindows socket.\n"
printf "# Bind mount current directory.\n"
printf "# Modify CMD to use --gui.\n\n"
pe 'docker run -it \
-e "DISPLAY=${DISPLAY}" \
-v /tmp/.X11-unix:/tmp/.X11-unix:ro \
-v `pwd`:`pwd` \
-w `pwd` \
emacdona/dockerdemo \
octave --gui'

wait
clear
printf "# Shebang line.\n\n"
pe 'cat ./plot.m'
pe 'cat ./octave.sh'

wait
clear
pe './plot.m'
