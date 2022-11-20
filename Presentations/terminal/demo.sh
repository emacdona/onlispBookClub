#!/usr/bin/env bash
set -eo pipefail

. demo-magic/demo-magic.sh

clear
wait
printf "# Build the container.\n"
printf "# Pass user/group info for current user.\n\n"
pe 'docker build -q 
--build-arg UID=$(id -u ${USER}) 
--build-arg GID=$(id -g ${USER}) 
--build-arg USERNAME="${USER}" 
--build-arg USERGROUP="${USER}" 
-t emacdona/dockerdemo:latest 
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
p 'docker run -it \
-e "DISPLAY=${DISPLAY}" \
-v /tmp/.X11-unix:/tmp/.X11-unix:ro \
-v `pwd`:`pwd` \
-w `pwd` \
emacdona/dockerdemo \
octave --gui'

# Actually run a slightly modified version that hides distracting errors.
docker run -it \
-e "DISPLAY=${DISPLAY}" \
-v /tmp/.X11-unix:/tmp/.X11-unix:ro \
-v `pwd`:`pwd` \
-w `pwd` \
emacdona/dockerdemo \
bash -c 'octave --gui 2>/dev/null'

wait
clear
printf "# Shebang line.\n\n"
pe 'cat ./plot.m'
pe 'cat ./octave.sh'

wait
clear
p './plot.m'

# Actually run a slightly modified version that hides distracting errors.
./plot.m 2>/dev/null
