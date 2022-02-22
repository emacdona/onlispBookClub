FROM ubuntu:20.04

# Re-enable man pages
RUN yes | unminimize

# Install packages via apt
RUN apt-get update

RUN DEBIAN_FRONTEND=noninteractive apt-get install -y git
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y screen
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
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y firefox
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y xfce4-terminal
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y thunar
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y supervisor
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y jq
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y dnsutils
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y iputils-ping
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y dia
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y libxml2-utils

# Required by spacemacs w3m layer
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y w3m

# required by demo-magic
# https://github.com/paxtonhare/demo-magic
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y pv

# Some math tools
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y octave
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y maxima wxmaxima
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y sagemath

# Sage seems to use straight up system python (boo!). Their site seems to indicate that maybe someday they'll have a well supported sage venv. Anyway, until
# then... gotta install "sage" specific things before we switch pythons via pyenv
# https://wiki.sagemath.org/ReleaseTours/sage-9.3#Alternative_installation_methods_using_pip
RUN pip install jupyterlab

# pyenv pre-reqs (delta from existing installed packages)
# https://github.com/pyenv/pyenv/wiki#suggested-build-environment
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y \
   libssl-dev \
   zlib1g-dev\
   libbz2-dev\
   libreadline-dev\
   libsqlite3-dev\
   wget\
   llvm\
   libncursesw5-dev\
   xz-utils\
   tk-dev\
   libxml2-dev\
   libxmlsec1-dev\
   libffi-dev\
   liblzma-dev

# https://www.xach.com/lisp/buildapp/
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y buildapp

# Install a Latex environment.
# (copied from the blang/latex ubuntu dockerfile)
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y texlive-full
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y python3-pygments
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y gnuplot
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y make

# added later...
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y texstudio

# Install scheme
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y mit-scheme mit-scheme-dbg mit-scheme-doc

# Emacs from source (because Ubuntul LTS emacs packages are too old for Spacemacs now)
# https://ubuntuhandbook.org/index.php/2021/12/compile-gnu-emacs-source-ubuntu/
# https://askubuntu.com/questions/158871/how-do-i-enable-the-source-code-repositories
RUN sed -i '/deb-src/s/^# //' /etc/apt/sources.list
RUN DEBIAN_FRONTEND=noninteractive apt-get update -y
RUN DEBIAN_FRONTEND=noninteractive apt-get build-dep emacs -y
RUN DEBIAN_FRONTEND=noninteractive apt-get install autoconf -y
RUN curl -sO https://ftp.gnu.org/gnu/emacs/emacs-27.2.tar.gz && \
    tar xfz emacs-27.2.tar.gz && \
    cd emacs-27.2 && \
    ./autogen.sh && \
    ./configure && \
    make bootstrap -j4 && \
    make install

# https://docs.docker.com/engine/install/ubuntu/ (but we're not installing the engine, just the cli)
RUN apt-get install -y apt-transport-https ca-certificates gnupg lsb-release
RUN curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg
RUN echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
RUN apt-get update
RUN apt-get install -y docker-ce-cli

# https://docs.mongodb.com/mongodb-shell/install/#std-label-mdb-shell-install
RUN wget -qO - https://www.mongodb.org/static/pgp/server-5.0.asc | apt-key add - && \
    echo "deb [ arch=amd64,arm64 ] https://repo.mongodb.org/apt/ubuntu focal/mongodb-org/5.0 multiverse" | tee /etc/apt/sources.list.d/mongodb-org-5.0.list && \
    apt-get update && \
    apt-get install -y mongodb-mongosh

# https://docs.mongodb.com/database-tools/installation/installation-linux/
# Note: download link specific to this version of Ubuntu
RUN curl -sO https://fastdl.mongodb.org/tools/db/mongodb-database-tools-ubuntu2004-x86_64-100.5.2.deb && \
    apt install ./mongodb-database-tools-*-100.5.2.deb

# Reddit data is compressed using this command.
RUN   apt-get install -y zstd

# dotnet core
# https://docs.microsoft.com/en-us/dotnet/core/install/linux-ubuntu#2004-
RUN wget https://packages.microsoft.com/config/ubuntu/20.04/packages-microsoft-prod.deb -O packages-microsoft-prod.deb && \
    dpkg -i packages-microsoft-prod.deb && \
    rm packages-microsoft-prod.deb

RUN DEBIAN_FRONTEND=noninteractive apt-get update -y && \
    apt-get install -y apt-transport-https && \
    apt-get install -y dotnet-sdk-6.0 && \
    apt-get install -y aspnetcore-runtime-6.0

# Terraform
# https://learn.hashicorp.com/tutorials/terraform/install-cli?in=terraform/aws-get-started
# https://phoenixnap.com/kb/add-apt-repository-command-not-found-ubuntu
RUN curl -fsSL https://apt.releases.hashicorp.com/gpg | sudo apt-key add - \
    sudo apt-get install -y software-properties-common \
    apt-add-repository "deb [arch=amd64] https://apt.releases.hashicorp.com $(lsb_release -cs) main" \
    apt-get update && sudo apt-get install -y terraform

# vscode
# https://code.visualstudio.com/docs/setup/linux
RUN wget -qO- https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > packages.microsoft.gpg && \
    install -o root -g root -m 644 packages.microsoft.gpg /etc/apt/trusted.gpg.d/ && \
    sh -c 'echo "deb [arch=amd64,arm64,armhf signed-by=/etc/apt/trusted.gpg.d/packages.microsoft.gpg] https://packages.microsoft.com/repos/code stable main" > /etc/apt/sources.list.d/vscode.list' && \
    rm -f packages.microsoft.gpg && \
    apt install apt-transport-https && \
    apt update && \
    apt install code

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

# on-lisp source code for modern lisps -- so we don't have to type in all of PGs function definitions,
# which he builds upon as the book progresses
RUN git clone https://github.com/DalekBaldwin/on-lisp.git

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
RUN sudo -i -u "${USERNAME}" bash -c 'source ~/.sdkman/bin/sdkman-init.sh &&  sdk install scala 3.1.0'
RUN sudo -i -u "${USERNAME}" bash -c 'source ~/.sdkman/bin/sdkman-init.sh &&  sdk install scala 2.13.7'
RUN sudo -i -u "${USERNAME}" bash -c 'source ~/.sdkman/bin/sdkman-init.sh &&  sdk install sbt 1.5.5'

# pyenv
# https://github.com/pyenv/pyenv
# Running the commands specified there to update shell configuration files was a bridge too far. Adding Dockerfile quoting syntax
# on top of shell quoting syntax is a disaster. Here, I'll just clone the repo and run the make command.
# I've hard coded the shell configuration in ~/.zshrc
RUN sudo -i -u "${USERNAME}" bash -c "git clone https://github.com/pyenv/pyenv.git ~/.pyenv"
RUN sudo -i -u "${USERNAME}" bash -c "cd ~/.pyenv && src/configure && make -C src"

# ABCL
# https://abcl.org/doc/abcl-user.html
# Note: keep an eye on the download link here: https://abcl.org/
# (Not sure how long outdated links will continue to work)
WORKDIR "/home/${USERNAME}/usr/src"
COPY --chown="${USERNAME}:${USERGROUP}" abcl-completions.lisp abcl-completions.lisp
RUN curl -O https://abcl.org/releases/1.8.0/abcl-src-1.8.0.tar.gz && \
    tar xvfz abcl-src-1.8.0.tar.gz && \
    cd abcl-src-1.8.0 && \
    bash -c 'source ~/.sdkman/bin/sdkman-init.sh && ant -f build.xml' && \
    bash -c 'source ~/.sdkman/bin/sdkman-init.sh && java -jar dist/abcl.jar --load ../abcl-completions.lisp'

# Haskell b/c wtf not? Nah, the emacs Dockerfile linter needs a program written in haskell.
# https://docs.haskellstack.org/en/stable/README/
RUN curl -sSL https://get.haskellstack.org/ | sh

# Aforementioned Dockerfile linter
# https://develop.spacemacs.org/layers/+tools/docker/README.html
# https://github.com/hadolint/hadolint
WORKDIR "/home/${USERNAME}/usr/src"
RUN git clone https://github.com/hadolint/hadolint \
   && cd hadolint \
   && stack install

RUN sudo -i -u "${USERNAME}" bash -c '\
      mkdir -p ~/usr/local && \
      cd ~/usr/local && \
      curl -sLO https://download.jetbrains.com/idea/ideaIU-2021.1.3.tar.gz && \
      tar xvfz ideaIU-2021.1.3.tar.gz'

WORKDIR "/home/${USERNAME}"

# https://k3d.io/v5.1.0/
RUN sudo -i -u "${USERNAME}" bash -c '\
    curl -s https://raw.githubusercontent.com/rancher/k3d/main/install.sh | TAG=v5.0.0 bash'

# https://kubernetes.io/docs/tasks/tools/install-kubectl-linux/
RUN sudo -i -u "${USERNAME}" bash -c '\
    curl -LO "https://dl.k8s.io/release/$(curl -L -s https://dl.k8s.io/release/stable.txt)/bin/linux/amd64/kubectl"'

RUN sudo install -o root -g root -m 0755 kubectl /usr/local/bin/kubectl

# https://helm.sh/docs/intro/install/
RUN sudo -i -u "${USERNAME}" bash -c '\
    curl https://raw.githubusercontent.com/helm/helm/main/scripts/get-helm-3 | bash'

# Copy in some dotfiles.
# Hell, mount them as volumes too when we start the container. That way, if you change them on the host and it doesn't cause this layer to
# rebuild, you'll still see your changes! (I'm not sure how to force this layer to rebuild if one of these files changes on the host).
# Note that the .emacs file HAS to be copied in because we count on it being there when we run emacs in batch mode while building the
# container -- which results in emacs plugins being installed ON A LAYER IN THE CONTAINER.
COPY --chown="${USERNAME}:${USERGROUP}" .exrc .exrc
COPY --chown="${USERNAME}:${USERGROUP}" .screenrc .screenrc
COPY --chown="${USERNAME}:${USERGROUP}" .zshrc .zshrc
COPY --chown="${USERNAME}:${USERGROUP}" .spacemacs .spacemacs
COPY --chown="${USERNAME}:${USERGROUP}" terminalrc .config/xfce4/terminal/terminalrc
COPY --chown="${USERNAME}:${USERGROUP}" supervisord.conf supervisord.conf

# Since we currently only have pyenv installed for zsh, we need to wait until our zshrc (with manual changes for pyenv) is installed so we can install pythons. Consider installing it for bash too, and
# running this earlier?
# Also, don't understand why I HAVE to source ~/.zshrc. But that's a problem for another time
RUN sudo -i -u "${USERNAME}" zsh -c 'source ~/.zshrc && pyenv install 3.10.2 && pyenv global 3.10.2'

# Installing ansible with a pyenv installed python doesn't seem to create an "ansible" shim... so you can't run it unless you know where it's at.
# Turns out that best practice is to install a virtualenv anyway, and then put ansible there.
# Probably a project specific venv is best, but for now, I'll create a "common" one for me to use
# https://docs.ansible.com/ansible/latest/installation_guide/intro_installation.html
RUN sudo -i -u "${USERNAME}" zsh -c 'source ~/.zshrc && python -m pip install virtualenv'
RUN sudo -i -u "${USERNAME}" zsh -c 'mkdir -p ~/venv'
RUN sudo -i -u "${USERNAME}" zsh -c 'source ~/.zshrc && cd ~/venv && python -m virtualenv ansible && . ansible/bin/activate && pip install ansible && deactivate'

# Installing Anaconda
RUN sudo -i -u "${USERNAME}" zsh -c '\
    curl -O https://repo.anaconda.com/archive/Anaconda3-2021.11-Linux-x86_64.sh && \
    chmod +x Anaconda3-2021.11-Linux-x86_64.sh && \
    zsh ./Anaconda3-2021.11-Linux-x86_64.sh -b && \
    rm Anaconda3-2021.11-Linux-x86_64.sh'

# https://anaconda.org/anaconda/pymongo
RUN sudo -i -u "${USERNAME}" zsh -c '~/anaconda3/bin/conda install -y -c anaconda pymongo'

# spacemacs
RUN git clone https://github.com/syl20bnr/spacemacs "/home/${USERNAME}/.emacs.d"

# https://github.com/venmos/w3m-layer
RUN git clone https://github.com/venmos/w3m-layer.git ~/.emacs.d/private/w3m

# Run emacs in batch mode to install all packages
RUN sudo -i -u "${USERNAME}" bash -c 'emacs -batch -l ~/.emacs.d/init.el'

# Install istoctl
# https://istio.io/latest/docs/setup/getting-started/
RUN sudo -i -u "${USERNAME}" bash -c "\
    curl -L https://istio.io/downloadIstio | sh - && \
    mv istio-* istio"

# For when I inevitably give up on emacs, set up vim in a way that I like ;-)
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

COPY --chown="${USERNAME}:${USERGROUP}" scripts scripts

RUN   sudo -i -u "${USERNAME}" bash -c 'curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.0/install.sh | bash \
      && . /home/${USER}/.nvm/nvm.sh \
      && nvm install node'

# Bazelisk
# https://github.com/bazelbuild/bazelisk/blob/master/README.md
RUN   sudo -i -u "${USERNAME}" bash -c '. /home/${USER}/.nvm/nvm.sh && npm install -g @bazel/bazelisk'

# mdp
# https://github.com/visit1985/mdp
RUN   sudo -i -u "${USERNAME}" bash -c 'cd ~/usr/src \
    && git clone https://github.com/visit1985/mdp.git \
    && cd mdp \
    && make \
    && sudo make install'

# ZAP
RUN cd "/home/${USERNAME}/usr/local" && \
    curl -OL https://github.com/zaproxy/zaproxy/releases/download/v2.11.1/ZAP_2.11.1_Linux.tar.gz && \
    tar xvfz ZAP_2.11.1_Linux.tar.gz

# Set up shell completion for some utils
RUN   sudo -i -u "${USERNAME}" bash -c 'mkdir ~/.oh-my-zsh/completions'
RUN   sudo -i -u "${USERNAME}" bash -c 'kubectl completion zsh > ~/.oh-my-zsh/completions/_kubectl'
RUN   sudo -i -u "${USERNAME}" bash -c 'helm completion zsh > ~/.oh-my-zsh/completions/_helm'
RUN   sudo -i -u "${USERNAME}" bash -c 'k3d completion zsh > ~/.oh-my-zsh/completions/_k3d'

WORKDIR "/home/${USERNAME}/onlisp"
env USER ${USERNAME}

CMD ["zsh" , "-c", "supervisord -n -c ${HOME}/supervisord.conf"]
