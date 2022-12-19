FROM ubuntu:20.04

# Re-enable man pages
RUN yes | unminimize

# Install packages via apt
RUN apt-get update


# QT Creator note:
# Why libclang-common-8-dev?
# https://bugreports.qt.io/browse/QTCREATORBUG-19667?focusedCommentId=508061&page=com.atlassian.jira.plugin.system.issuetabpanels%3Acomment-tabpanel#comment-508061

# pympress install instructions (with dependencies)
# https://github.com/Cimbali/pympress

# gvm dependencies
# https://github.com/moovweb/gvm

RUN DEBIAN_FRONTEND=noninteractive apt-get install -y git \
   screen \
   vim \
   sbcl \
   zsh \
   curl \
   unzip \
   zip \
   sudo \
   clisp \
   texinfo \
   build-essential \
   python3-sphinx \
   locales \
   rlwrap \
   firefox \
   xfce4-terminal \
   thunar \
   supervisor \
   jq \
   dnsutils \
   iputils-ping \
   dia \
   libxml2-utils \
   uuid-runtime \
   usbutils \
   inkscape \
   libgtk2.0-dev \
   libgtkglext1-dev \
   openssl \
   libssl-dev \
   qtcreator \
   qt5-default \
   libclang-common-8-dev \
   kile \
   pympress libgtk-3-0 libpoppler-glib8 libcairo2 python3-gi python3-gi-cairo gobject-introspection libgirepository-1.0-1 gir1.2-gtk-3.0 gir1.2-poppler-0.18 \
   bison mercurial

# Required by spacemacs w3m layer
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y w3m

# required by demo-magic
# https://github.com/paxtonhare/demo-magic
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y pv

# Some math tools
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y octave
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y maxima wxmaxima
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y sagemath

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
RUN apt-get install -y docker-ce-cli docker-compose-plugin

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
RUN curl -fsSL https://apt.releases.hashicorp.com/gpg | apt-key add - && \
    apt-get install -y software-properties-common && \
    apt-add-repository "deb [arch=amd64] https://apt.releases.hashicorp.com $(lsb_release -cs) main" && \
    apt-get update && sudo apt-get install -y terraform

# AWS CLI
# https://docs.aws.amazon.com/cli/latest/userguide/getting-started-install.html
RUN cd /tmp && \
    curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "awscliv2.zip" && \
    unzip awscliv2.zip && \
    sudo ./aws/install

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
ARG HOME_DIR

RUN groupadd -g "${GID}" "${USERGROUP}"
RUN groupadd -g "${DOCKER_GID}" "docker"

RUN useradd \
   -d "${HOME_DIR}" \
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
RUN mkdir -p "${HOME_DIR}/usr/src/quicklisp"
WORKDIR "${HOME_DIR}/usr/src/quicklisp"
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp && \
    curl -O https://beta.quicklisp.org/quicklisp.lisp.asc
#    gpg --verify quicklisp.lisp.asc quicklisp.lisp

COPY --chown="${USERNAME}:${USERGROUP}" quicklisp-install-script.lisp quicklisp-install-script.lisp

# 'echo |' to get past the "enter" prompt at end of install
RUN echo | sbcl --load quicklisp.lisp --script quicklisp-install-script.lisp

# clon (required by erudite) currently having problems installing via quicklisp b/c of missing package. See:
# https://githubmemory.com/repo/didierverna/clon/issues/9
# So, install manually: (git clone clon... lolz)
WORKDIR "${HOME_DIR}/quicklisp/local-projects"
RUN git clone https://github.com/didierverna/clon.git

# on-lisp source code for modern lisps -- so we don't have to type in all of PGs function definitions,
# which he builds upon as the book progresses
RUN git clone https://github.com/DalekBaldwin/on-lisp.git

# Erudite
WORKDIR "${HOME_DIR}/usr/src"
RUN git clone https://github.com/mmontone/erudite.git

WORKDIR "${HOME_DIR}/usr/src/erudite"
RUN make && sudo make install

WORKDIR "${HOME_DIR}"

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
RUN sudo -i -u "${USERNAME}" bash -c 'source ~/.sdkman/bin/sdkman-init.sh &&  sdk install kotlin 1.6.21'
RUN sudo -i -u "${USERNAME}" bash -c 'source ~/.sdkman/bin/sdkman-init.sh &&  sdk install leiningen 2.9.7'
RUN sudo -i -u "${USERNAME}" bash -c 'source ~/.sdkman/bin/sdkman-init.sh &&  sdk install scala 3.1.0'
RUN sudo -i -u "${USERNAME}" bash -c 'source ~/.sdkman/bin/sdkman-init.sh &&  sdk install scala 2.13.7'
RUN sudo -i -u "${USERNAME}" bash -c 'source ~/.sdkman/bin/sdkman-init.sh &&  sdk install sbt 1.5.5'
RUN sudo -i -u "${USERNAME}" bash -c 'source ~/.sdkman/bin/sdkman-init.sh &&  sdk install spark 3.2.1'

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
WORKDIR "${HOME_DIR}/usr/src"
COPY --chown="${USERNAME}:${USERGROUP}" abcl-completions.lisp abcl-completions.lisp
RUN curl -O https://abcl.org/releases/1.9.0/abcl-src-1.9.0.tar.gz && \
    tar xvfz abcl-src-1.9.0.tar.gz && \
    cd abcl-src-1.9.0 && \
    bash -c 'source ~/.sdkman/bin/sdkman-init.sh && ant -f build.xml' && \
    bash -c 'source ~/.sdkman/bin/sdkman-init.sh && java -jar dist/abcl.jar --load ../abcl-completions.lisp'

# Haskell b/c wtf not? Nah, the emacs Dockerfile linter needs a program written in haskell.
# https://docs.haskellstack.org/en/stable/README/
RUN curl -sSL https://get.haskellstack.org/ | sh

# Aforementioned Dockerfile linter
# https://develop.spacemacs.org/layers/+tools/docker/README.html
# https://github.com/hadolint/hadolint
WORKDIR "${HOME_DIR}/usr/src"
RUN git clone https://github.com/hadolint/hadolint \
   && cd hadolint \
   && git fetch --all --tags \
   && git checkout tags/v2.10.0 -b v2.10.0 \
   && stack install

# I currently have a bug submitted for this: https://github.com/hadolint/hadolint/issues/904
# Until it's fixed, I won't build from master, but I'll use a tag that works (above)
## hadolint switched from stack to cabal...
## https://www.haskell.org/ghcup/guide/
## https://www.haskell.org/cabal/
## https://stackoverflow.com/questions/72952659/how-to-do-unattended-haskell-installation
#RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
#   BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
#   BOOTSTRAP_HASKELL_GHC_VERSION=latest \
#   BOOTSTRAP_HASKELL_CABAL_VERSION=latest \
#   BOOTSTRAP_HASKELL_INSTALL_STACK=1 \
#   BOOTSTRAP_HASKELL_INSTALL_HLS=1 \
#   BOOTSTRAP_HASKELL_ADJUST_BASHRC=P \
#   sh
#
#RUN git clone https://github.com/hadolint/hadolint \
#   && cd hadolint \
#   && . "${HOME_DIR}/.ghcup/env" \
#   && cabal configure \
#   && cabal build \
#   && cabal install

RUN sudo -i -u "${USERNAME}" bash -c '\
      mkdir -p ~/usr/local && \
      cd ~/usr/local && \
      curl -sLO https://download.jetbrains.com/idea/ideaIU-2022.1.3.tar.gz && \
      tar xvfz ideaIU-2022.1.3.tar.gz'

WORKDIR "${HOME_DIR}"

# https://k3d.io/v5.1.0/
RUN sudo -i -u "${USERNAME}" bash -c '\
    curl -s https://raw.githubusercontent.com/rancher/k3d/main/install.sh | TAG=v5.4.6 bash'

# https://kubernetes.io/docs/tasks/tools/install-kubectl-linux/
RUN sudo -i -u "${USERNAME}" bash -c '\
    curl -LO "https://dl.k8s.io/release/$(curl -L -s https://dl.k8s.io/release/stable.txt)/bin/linux/amd64/kubectl"'

RUN sudo install -o root -g root -m 0755 kubectl /usr/local/bin/kubectl

# https://helm.sh/docs/intro/install/
RUN sudo -i -u "${USERNAME}" bash -c '\
    curl https://raw.githubusercontent.com/helm/helm/main/scripts/get-helm-3 | bash'

# Ansible asks for this plugin
# https://github.com/databus23/helm-diff
RUN   sudo -i -u "${USERNAME}" bash -c 'helm plugin install https://github.com/databus23/helm-diff'

# RVM: https://rvm.io/
# Jekyll: https://jekyllrb.com/
# Note: rvm install will modify zshrc (and zlogin, but ignore that). My plan is to install it here, source it explicitly, then install ruby and jekyll. Then
# I'm going to copy over top of .zshrc in the container with the one I have in vc. But I've made the same changes to that file that's in vc.
RUN zsh -c '\
    gpg --recv-keys 409B6B1796C275462A1703113804BB82D39DC0E3 7D2BAF1CF37B13E2069D6956105BD0E739499BDB && \
    curl -sSL https://get.rvm.io | bash -s stable && \
    source "$HOME/.rvm/scripts/rvm" && \
    rvm install ruby-3.0.0 && \
    rvm use ruby-3.0.0 && \
    gem install bundler jekyll'

# Copy in some dotfiles.
# Hell, mount them as volumes too when we start the container. That way, if you change them on the host and it doesn't cause this layer to
# rebuild, you'll still see your changes! (I'm not sure how to force this layer to rebuild if one of these files changes on the host).
# Note that the .emacs file HAS to be copied in because we count on it being there when we run emacs in batch mode while building the
# container -- which results in emacs plugins being installed ON A LAYER IN THE CONTAINER.
COPY --chown="${USERNAME}:${USERGROUP}" .exrc .exrc
COPY --chown="${USERNAME}:${USERGROUP}" .screenrc .screenrc
COPY --chown="${USERNAME}:${USERGROUP}" .zshrc .zshrc
COPY --chown="${USERNAME}:${USERGROUP}" .spacemacs .spacemacs
COPY --chown="${USERNAME}:${USERGROUP}" supervisord.conf supervisord.conf

# Couldn't find any combination of command line params and arguments that would let me install quicklisp for ABCL. So, since it's already installed b/c of SBCL, I'm just going to steal
# the .abclrc file that (ql:add-to-init-file) generates (see: https://www.quicklisp.org/beta/faq.html)
COPY --chown="${USERNAME}:${USERGROUP}" .abclrc .abclrc

# gvm, lazydocker
# https://github.com/moovweb/gvm (though I subbed zsh for bash)
# https://github.com/jesseduffield/lazydocker/blob/master/docs/Config.md
RUN zsh -c 'zsh < <(curl -s -S -L https://raw.githubusercontent.com/moovweb/gvm/master/binscripts/gvm-installer)' && \
    sudo -i -u "${USERNAME}" zsh -ic 'gvm install go1.4 -B' && \
    sudo -i -u "${USERNAME}" zsh -ic 'gvm use go1.4 --default' && \
    sudo -i -u "${USERNAME}" zsh -ic 'gvm install go1.19.3' && \
    sudo -i -u "${USERNAME}" zsh -ic 'gvm use go1.19.3 --default' && \
    sudo -i -u "${USERNAME}" zsh -ic 'go install github.com/jesseduffield/lazydocker@latest'

# Perl
# We need to install perl HERE so that the perlbrew line we source in the .zshrc file doesn't result in an error when
# we use 'zsh' instead of 'bash' to install python via pyenv below
RUN   sudo -i -u "${USERNAME}" bash -c 'curl -L https://install.perlbrew.pl | bash'
RUN   sudo -i -u "${USERNAME}" bash -c 'source ~/perl5/perlbrew/etc/bashrc && perlbrew install perl-5.34.0 -Dusethreads --as perl-5.34.0_threads'
RUN   sudo -i -u "${USERNAME}" bash -c 'source ~/perl5/perlbrew/etc/bashrc && perlbrew switch perl-5.34.0_threads'

# Since we currently only have pyenv installed for zsh, we need to wait until our zshrc (with manual changes for pyenv) is installed so we can install pythons. Consider installing it for bash too, and
# running this earlier?
RUN sudo -i -u "${USERNAME}" zsh -ic 'pyenv install 3.10.2 && pyenv global 3.10.2'

# This seems like as good a place as any to put python packages we want to have installed "globally" (for our own global python).
# python-language-server: used by spacemacs
#
# Note: I went with this yq: https://github.com/kislyuk/yq
# because this article: https://towardsdatascience.com/yq-mastering-yaml-processing-in-command-line-e1ff5ebc0823
# Billed it as the one being "more like jq"
RUN sudo -i -u "${USERNAME}" zsh -ic 'pip install python-language-server yq'

# Installing ansible with a pyenv installed python doesn't seem to create an "ansible" shim... so you can't run it unless you know where it's at.
# Turns out that best practice is to install a virtualenv anyway, and then put ansible there.
# Probably a project specific venv is best, but for now, I'll create a "common" one for me to use
# https://docs.ansible.com/ansible/latest/installation_guide/intro_installation.html
RUN sudo -i -u "${USERNAME}" zsh -ic 'python -m pip install virtualenv'
RUN sudo -i -u "${USERNAME}" zsh -ic 'mkdir -p ~/venv'
RUN sudo -i -u "${USERNAME}" zsh -ic 'cd ~/venv && python -m virtualenv ansible && . ansible/bin/activate && pip install ansible kubernetes && deactivate'

# Installing Mambaforge/Miniforge (per sage install instructions: https://doc.sagemath.org/html/en/installation/conda.html#sec-installation-conda)
RUN sudo -i -u "${USERNAME}" zsh -c '\
    cd /tmp && \
    curl -L -O https://github.com/conda-forge/miniforge/releases/latest/download/Mambaforge-$(uname)-$(uname -m).sh && \
    sh Mambaforge-$(uname)-$(uname -m).sh -b && \
    ~/mambaforge/condabin/conda init zsh'

RUN sudo -i -u "${USERNAME}" zsh -ic '\
    conda install mamba -y'

# Install a "sage" environment with other packages
RUN sudo -i -u "${USERNAME}" zsh -ic '\
    mamba create -n sage sage \
      jupyter jupyterlab spyder pymongo \
      python=3.10 -y'

# spacemacs
RUN git clone https://github.com/syl20bnr/spacemacs "${HOME_DIR}/.emacs.d"

# https://github.com/venmos/w3m-layer
RUN git clone https://github.com/venmos/w3m-layer.git ~/.emacs.d/private/w3m

# Run emacs in batch mode to install all packages
RUN sudo -i -u "${USERNAME}" bash -c 'emacs -batch -l ~/.emacs.d/init.el'

# Tell spacemacs to use the path we get from .zshrc (because of all of the shenanigans we've done with tools like sdk, pyenv, etc)
# The missing '-i' is why we had to source zshrc above. Need to think more before I decide to add it everywhere I'm doing that.
RUN sudo -i -u "${USERNAME}" zsh -ic '\
   printf "\n\n# Installed by Dockerfile\n" >> "${HOME}/.spacemacs.env" && \
   env | grep "^PATH=" >> "${HOME}/.spacemacs.env"'

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
RUN cd "${HOME_DIR}/usr/local" && \
    curl -OL https://github.com/zaproxy/zaproxy/releases/download/v2.11.1/ZAP_2.11.1_Linux.tar.gz && \
    tar xvfz ZAP_2.11.1_Linux.tar.gz

# Squeak
RUN cd "${HOME_DIR}/usr/local" && \
    curl -sOL https://files.squeak.org/5.3/Squeak5.3-19481-64bit/Squeak5.3-19481-64bit-202206021410-Linux-x64.tar.gz && \
    tar xvfz Squeak5.3-19481-64bit-202206021410-Linux-x64.tar.gz

# Dr Racket
# https://download.racket-lang.org/
# https://github.com/takikawa/drracket-vim-tool
RUN cd "${HOME_DIR}/usr/local" && \
    curl -sOL https://download.racket-lang.org/installers/8.6/racket-8.6-x86_64-linux-cs.sh && \
    chmod +x racket-8.6-x86_64-linux-cs.sh && \
    ./racket-8.6-x86_64-linux-cs.sh --in-place --dest ./racket && \
    "${HOME_DIR}/usr/local/racket/bin/raco" pkg install drracket-vim-tool

# Pharo
# https://pharo.org/download
RUN cd "${HOME_DIR}/usr/local" && mkdir pharo && cd pharo && \
    curl -sOL http://files.pharo.org/get-files/100/pharo-vm-Linux-x86_64-stable.zip && \
    curl -sOL https://files.pharo.org/get-files/100/pharo64.zip && \
    unzip pharo-vm-Linux-x86_64-stable.zip && \
    unzip pharo64.zip && \
    ln -s Pharo*.image Pharo.image

# Factor
# https://pharo.org/download
# Note: no close button on windows (use Alt + F4 to close them)
# https://github.com/factor/factor/issues/2265
# Also note: In the future, you may want to be more clever about how this starts up. By default, it seems to create a working directory
# as a descendant of the install directory (just work through the tutorial, you'll see what I mean)
RUN cd "${HOME_DIR}/usr/local" && \
    curl -sOL https://downloads.factorcode.org/releases/0.98/factor-linux-x86-64-0.98.tar.gz && \
    tar xvfz factor-linux-x86-64-0.98.tar.gz

# insomnia
RUN cd "/tmp" && \
    curl -sLo Insomnia.deb https://updates.insomnia.rest/downloads/ubuntu/latest\?\&app\=com.insomnia.app\&source\=website && \
    sudo dpkg -i Insomnia.deb

# K9s: https://github.com/derailed/k9s
RUN sudo -i -u "${USERNAME}" zsh -ic '\
    curl -sS https://webinstall.dev/k9s | zsh'


# Set up shell completion for some utils
RUN   sudo -i -u "${USERNAME}" bash -c 'mkdir ~/.oh-my-zsh/completions'
RUN   sudo -i -u "${USERNAME}" bash -c 'kubectl completion zsh > ~/.oh-my-zsh/completions/_kubectl'
RUN   sudo -i -u "${USERNAME}" bash -c 'helm completion zsh > ~/.oh-my-zsh/completions/_helm'
RUN   sudo -i -u "${USERNAME}" bash -c 'k3d completion zsh > ~/.oh-my-zsh/completions/_k3d'

WORKDIR "${HOME_DIR}/onlisp"
env USER ${USERNAME}

CMD ["zsh" , "-c", "supervisord -n -c ${HOME}/supervisord.conf"]
