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
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y texinfo
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y build-essential
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y python3-sphinx


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

RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> \
/etc/sudoers

USER "${USERNAME}"
WORKDIR "/home/${USERNAME}/onlisp"

ARG GIT_USER_EMAIL
ARG GIT_USER_NAME

RUN sudo -i -u "${USERNAME}" bash -c "git config --global user.email ${GIT_USER_EMAIL}"
RUN sudo -i -u "${USERNAME}" bash -c "git config --global user.name ${GIT_USER_NAME}"

# Oh My Zsh
# https://ohmyz.sh/#install
RUN sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

# SDK Man
# https://sdkman.io/install
RUN curl -s "https://get.sdkman.io" | bash
RUN sudo -i -u "${USERNAME}" bash -c 'source ~/.sdkman/bin/sdkman-init.sh &&  sdk install java 11.0.11.hs-adpt'

# Copy in some dotfiles.
# Hell, mount them as volumes too when we start the container. That way, if you change them on the host and it doesn't cause this layer to
# rebuild, you'll still see your changes! (I'm not sure how to force this layer to rebuild if one of these files changes on the host).
# Note that the .emacs file HAS to be copied in because we count on it being there when we run emacs in batch mode while building the
# container -- which results in emacs plugins being installed ON A LAYER IN THE CONTAINER.
COPY --chown="${USERNAME}:${USERGROUP}" .exrc .exrc
COPY --chown="${USERNAME}:${USERGROUP}" .screenrc .screenrc
COPY --chown="${USERNAME}:${USERGROUP}" .zshrc .zshrc
COPY --chown="${USERNAME}:${USERGROUP}" .emacs "/home/${USERNAME}/.emacs"

# Run emacs in batch mode to install all packages
RUN sudo -i -u "${USERNAME}" bash -c 'emacs --batch -l ~/.emacs'

# Why do they make this so hard?? Modified from: 
# https://github.com/dimitri/el-get
#RUN sudo -i -u "${USERNAME}" bash -c 'curl -s "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el" > /tmp/el-get-install.el'
#RUN sudo -i -u "${USERNAME}" bash -c 'emacs -batch -l /tmp/el-get-install.el'

#RUN sudo -i -u "${USERNAME}" bash -c 'emacs -batch -l /home/${USER}/.emacs'

# pathogen
RUN sudo -i -u "${USERNAME}" bash -c '\
   mkdir -p ~/.vim/autoload ~/.vim/bundle && \
   curl -LSso ~/.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim'

# Fugitive
RUN sudo -i -u "${USERNAME}" bash -c '\
   mkdir -p ~/.vim/pack/tpope/start && \
   cd ~/.vim/pack/tpope/start && \
   git clone https://tpope.io/vim/fugitive.git && \
   vim -u NONE -c "helptags fugitive/doc" -c q'

CMD zsh
