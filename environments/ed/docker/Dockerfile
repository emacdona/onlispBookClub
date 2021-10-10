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
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y locales
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y rlwrap

# https://docs.docker.com/engine/install/ubuntu/ (but we're not installing the engine, just the cli)
RUN apt-get install -y apt-transport-https ca-certificates gnupg lsb-release
RUN curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg
RUN echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
RUN apt-get update
RUN apt-get install -y docker-ce-cli


# https://unix.stackexchange.com/questions/90772/first-characters-of-the-command-repeated-in-the-display-when-completing
# https://askubuntu.com/questions/581458/how-to-configure-locales-to-unicode-in-a-docker-ubuntu-14-04-container
# Set the locale
RUN locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8


# Create a user corresponding to the host user
ARG UID
ARG USERNAME
ARG USERGROUP
ARG GID
ARG DOCKER_GID

RUN groupadd -g "${GID}" "${USERGROUP}"
RUN groupadd -g "${DOCKER_GID}" "docker"

RUN useradd \
   -d "/home/${USERNAME}" \
   -ms /bin/zsh \
   -g "${USERGROUP}" \
   -G sudo,docker \
   -u $UID "${USERNAME}"

RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> \
/etc/sudoers

USER "${USERNAME}"

ARG GIT_USER_EMAIL
ARG GIT_USER_NAME

RUN sudo -i -u "${USERNAME}" bash -c "git config --global user.email ${GIT_USER_EMAIL}"
RUN sudo -i -u "${USERNAME}" bash -c "git config --global user.name ${GIT_USER_NAME}"

# Quicklsip
RUN mkdir -p "/home/${USERNAME}/usr/src/quicklisp"
WORKDIR "/home/${USERNAME}/usr/src/quicklisp"
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp && \
    curl -O https://beta.quicklisp.org/quicklisp.lisp.asc 
#    gpg --verify quicklisp.lisp.asc quicklisp.lisp

COPY --chown="${USERNAME}:${USERGROUP}" quicklisp-install-script.lisp quicklisp-install-script.lisp

# 'echo |' to get past the "enter" prompt at end of install
RUN echo | sbcl --load quicklisp.lisp --script quicklisp-install-script.lisp

# clon (required by erudite) currently having problems installing via quicklisp b/c of missing package. See:
# https://githubmemory.com/repo/didierverna/clon/issues/9
# So, install manually: (git clone clon... lolz)
WORKDIR "/home/${USERNAME}/quicklisp/local-projects"
RUN git clone https://github.com/didierverna/clon.git

# Erudite
WORKDIR "/home/${USERNAME}/usr/src"
RUN git clone https://github.com/mmontone/erudite.git

WORKDIR "/home/${USERNAME}/usr/src/erudite"
RUN make && sudo make install

WORKDIR "/home/${USERNAME}"

# Oh My Zsh
# https://ohmyz.sh/#install
RUN sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

# SDK Man
# https://sdkman.io/install
RUN curl -s "https://get.sdkman.io" | bash
RUN sudo -i -u "${USERNAME}" bash -c 'source ~/.sdkman/bin/sdkman-init.sh &&  sdk install java 11.0.11.hs-adpt'
RUN sudo -i -u "${USERNAME}" bash -c 'source ~/.sdkman/bin/sdkman-init.sh &&  sdk install gradle 7.2'
RUN sudo -i -u "${USERNAME}" bash -c 'source ~/.sdkman/bin/sdkman-init.sh &&  sdk install maven 3.8.3'
RUN sudo -i -u "${USERNAME}" bash -c 'source ~/.sdkman/bin/sdkman-init.sh &&  sdk install ant 1.10.10'
RUN sudo -i -u "${USERNAME}" bash -c 'source ~/.sdkman/bin/sdkman-init.sh &&  sdk install kotlin 1.5.31'
RUN sudo -i -u "${USERNAME}" bash -c 'source ~/.sdkman/bin/sdkman-init.sh &&  sdk install leiningen 2.9.7'

# ABCL
# https://abcl.org/doc/abcl-user.html
# Note: keep an eye on the download link here: https://abcl.org/
# (Not sure how long outdated links will continue to work)
WORKDIR "/home/${USERNAME}/usr/src"
RUN curl -O https://abcl.org/releases/1.8.0/abcl-src-1.8.0.tar.gz && \
    tar xvfz abcl-src-1.8.0.tar.gz && \
    cd abcl-src-1.8.0 && \
    bash -c 'source ~/.sdkman/bin/sdkman-init.sh && ant -f build.xml'

WORKDIR "/home/${USERNAME}"

# Copy in some dotfiles.
# Hell, mount them as volumes too when we start the container. That way, if you change them on the host and it doesn't cause this layer to
# rebuild, you'll still see your changes! (I'm not sure how to force this layer to rebuild if one of these files changes on the host).
# Note that the .emacs file HAS to be copied in because we count on it being there when we run emacs in batch mode while building the
# container -- which results in emacs plugins being installed ON A LAYER IN THE CONTAINER.
COPY --chown="${USERNAME}:${USERGROUP}" .exrc .exrc
COPY --chown="${USERNAME}:${USERGROUP}" .screenrc .screenrc
COPY --chown="${USERNAME}:${USERGROUP}" .zshrc .zshrc
COPY --chown="${USERNAME}:${USERGROUP}" .emacs .emacs

# The only emacs solarized theme I could get working for terminal based emacs
RUN git clone https://github.com/sellout/emacs-color-theme-solarized.git ~/.emacs.d/emacs-color-theme-solarized

# Run emacs in batch mode to install all packages
RUN sudo -i -u "${USERNAME}" bash -c 'emacs --batch -l ~/.emacs'

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

WORKDIR "/home/${USERNAME}/onlisp"

CMD zsh
