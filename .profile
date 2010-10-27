# General {
    # Init Fink on OSX
    if [ -d /sw/fink ]; then
        . /sw/bin/init.sh
    fi
# }

# Path {
    # Set path for MacPorts
    export PATH=/opt/local/bin:/opt/local/sbin:$PATH
# }

# History {
    export HISTCONTROL=ignoredups
    shopt -s histappend
    HISTIGNORE='??'
# }

# Aliases {
    alias ..="cd .."
    alias ls="ls -lhG"
    alias untar="tar xzfv"
# }

# Functions {
    # mkdir, cd into it
    mkcd () {
        mkdir -p "$*"
        cd "$*"
    }
# }

# Variables {
    # Editor
    export VISUAL='vim'
    export EDITOR=$VISUAL
    export SVN_EDITOR=$VISUAL

    # Tab completion ignore pattern
    export FIGNORE=.svn
# }


