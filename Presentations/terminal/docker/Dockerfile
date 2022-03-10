FROM ubuntu:20.04

RUN apt-get update && \
   DEBIAN_FRONTEND=noninteractive \
   apt-get install -y octave

ARG UID
ARG GID
ARG USERNAME
ARG USERGROUP

RUN groupadd -g "${GID}" "${USERGROUP}"

RUN useradd \
    -d "/home/${USERNAME}" \
    -ms /bin/zsh \
    -g "${USERGROUP}" \
    -u $UID "${USERNAME}"

RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> \
    /etc/sudoers

USER "${USERNAME}"

WORKDIR "/home/${USERNAME}/workdir"

COPY myPlot.m myPlot.m

CMD ["octave", "--no-gui", "--quiet"]
