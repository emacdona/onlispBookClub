FROM ubuntu:20.04

# Install packages via apt
RUN apt-get update

RUN DEBIAN_FRONTEND=noninteractive apt-get install -y git
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y screen
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y emacs
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y vim
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y sbcl
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y zsh
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y curl
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y unzip
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y zip
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y sudo
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y clisp


# Create a user corresponding to the host user
ARG UID
ARG USERNAME
ARG USERGROUP
ARG GID

RUN groupadd -g "${GID}" "${USERGROUP}"

RUN useradd \
   -d "/home/${USERNAME}" \
   -ms /bin/zsh \
   -g "${USERGROUP}" \
   -G sudo \
   -u $UID "${USERNAME}"

USER "${USERNAME}"
WORKDIR "/home/${USERNAME}/onlisp"

# Oh My Zsh
# https://ohmyz.sh/#install
RUN sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

# SDK Man
# https://sdkman.io/install
RUN curl -s "https://get.sdkman.io" | bash
RUN sudo -i -u "${USERNAME}" bash -c 'source /home/${USER}/.sdkman/bin/sdkman-init.sh &&  sdk install java 11.0.11.hs-adpt'

# pathogen
RUN sudo -i -u "${USERNAME}" bash -c '\
   mkdir -p ~/.vim/autoload ~/.vim/bundle && \
   curl -LSso ~/.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim'

# Copy in some dotfiles
COPY --chown="${USERNAME}:${USERGROUP}" .exrc .exrc
COPY --chown="${USERNAME}:${USERGROUP}" .screenrc .screenrc

CMD zsh
