# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="/home/${USER}/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="robbyrussell"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following lines to change the auto-update behavior
# zstyle ':omz:update' mode disabled  # disable automatic updates
# zstyle ':omz:update' mode auto      # update automatically without asking
# zstyle ':omz:update' mode reminder  # just remind me to update when it's time

# Uncomment the following line to change how often to auto-update (in days).
# zstyle ':omz:update' frequency 13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# You can also set it to another string to have that shown instead of the default red dots.
# e.g. COMPLETION_WAITING_DOTS="%F{yellow}waiting...%f"
# Caution: this setting can cause issues with multiline prompts in zsh < 5.7.1 (see #5765)
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git terraform)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

export PATH="/home/${USER}/scripts:/home/${USER}/bin:${PATH}"

# Tell screen to use unicode.
alias screen="screen -U"

# My favorite git history viewing command
alias gg="git log --oneline --graph --all"

# DON'T PAGE IF IT'S LESS THAN A PAGE!!!
export LESS="-F -X $LESS"

# Given I can never remember to type "rlwrap ...", maybe I have a better chance
# at remembering to type "repl"
alias repl="rlwrap sbcl"

# So I don't forget to add the --api-port switch that will let me connect to the cluster from other containers
alias k8scluster="k3d cluster create --api-port host.docker.internal:42042"

alias dockerRepoPort="docker ps -f name=registry --format \"{{json .}}\" | jq -r '.Ports' | perl -ne 'print((split(/:/, (split(/->/))[0]))[1], \"\n\")'"

# repls for individual lisps. May be better to write scripts like the one I yanked from the ABCL
# documentation for sbcl and clisp (it's smart: it lets you just type "abcl" and gives you a repl
# if there are no arguments)
alias sbclr="rlwrap sbcl"
alias clispr="rlwrap clisp"
alias abclr="abcl"

# my "global" ansible venv
alias ansible-venv=". ~/venv/ansible/bin/activate"

# Just to save me some typing for now (until I think of a better way to do this; like automate setting up a user account in mongodb?)
alias mongofwd='kubectl port-forward --namespace mongodb svc/mongodb 27017:27017 &'
alias mongopw='echo $(kubectl get secret --namespace mongodb mongodb -o jsonpath="{.data.mongodb-root-password}" | base64 --decode)'
alias mongosh='mongosh --host 127.0.0.1 --username root --password $(mongopw)'
alias jenkinspw=$'kubectl get secret jenkins --output json | jq -r \'.data."jenkins-admin-password"\' | base64 -d'
alias gitlabpw=$'kubectl get secret gitlab-gitlab-initial-root-password -ojsonpath=\'{.data.password}\' | base64 --decode ; echo'

# Get the kube dashboard token
alias dashtoken='kubectl -n kubernetes-dashboard get secret $(kubectl -n kubernetes-dashboard get sa/admin-user -o jsonpath="{.secrets[0].name}") -o go-template="{{.data.token | base64decode}}"'

# Show the cert that kubectl is using to connect to k8s
# Cert chains in PEM aren't handled well by openssl when rendering them to text. Convert to pkcs7 first.
# https://unix.stackexchange.com/questions/696224/using-openssl-to-display-all-certificates-of-a-pem-file
alias kubectlcert=$'openssl crl2pkcs7 -nocrl -certfile <(cat ~/.kube/config | yq "." | jq -r \'.users[0].user."client-certificate-data"\' | base64 -d) | openssl pkcs7 -print_certs -text -noout'

# Add the dashtoken to the kubectl config so that we can use it to authenticate when using the dashboard
alias kubectltoken=$'yq -yi --arg TOKEN "$(dashtoken)" \'.users |= [.[] | if .name == "admin@k3d-k3s-default" then . | .user.token = $TOKEN  else . end]\' ~/.kube/config'

# 'supervisorctl' is way too much to type
alias sc="supervisorctl --configuration ~/supervisord.conf"


set -o vi

# haskell "stack?" installs stuff here
export PATH="${HOME}/.local/bin:$PATH"

# istioctl
export PATH="${HOME}/istio/bin:${PATH}"


#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# perlbrew
[ -f "${HOME}/perl5/perlbrew/etc/bashrc" ] && source "${HOME}/perl5/perlbrew/etc/bashrc"

# Pyenv, hard coded by me. Tailoring these instructions to work inside a Dockerfile was just too much
# (quoting issues). Also, why the f*ck do they insist on login shells. I'm putting _everything_ here.
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init --path)"
eval "$(pyenv init -)"

autoload -U +X bashcompinit && bashcompinit
complete -o nospace -C /usr/bin/terraform terraform

# https://docs.aws.amazon.com/cli/latest/userguide/cli-configure-completion.html#cli-command-completion-linux
complete -C '/usr/local/bin/aws_completer' aws

# Source gvm. We check if it exists first so that this won't cause shell initialization to fail when building the
# docker container (ie: before we've installed gvm)
[ -f "${HOME}/.gvm/scripts/gvm" ] && source "${HOME}/.gvm/scripts/gvm"


# Generated for envman. Do not edit.
[ -s "$HOME/.config/envman/load.sh" ] && source "$HOME/.config/envman/load.sh"



# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/emacdona/mambaforge/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/emacdona/mambaforge/etc/profile.d/conda.sh" ]; then
        . "/home/emacdona/mambaforge/etc/profile.d/conda.sh"
    else
        export PATH="/home/emacdona/mambaforge/bin:$PATH"
    fi
fi
unset __conda_setup

if [ -f "/home/emacdona/mambaforge/etc/profile.d/mamba.sh" ]; then
    . "/home/emacdona/mambaforge/etc/profile.d/mamba.sh"
fi
# <<< conda initialize <<<

# rvm puts this in ~/.zlogin. I'm putting it here
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"
