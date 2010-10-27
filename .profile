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

# Prompt {
    D=$'\e[37;40m'
    PINK=$'\e[35;40m'
    GREEN=$'\e[32;40m'
    ORANGE=$'\e[33;40m'

    function prompt_char {
        git branch >/dev/null 2>/dev/null && echo '±' && return
        svn info >/dev/null 2>/dev/null && echo '✔' && return
        echo '○'
    }

    export PS1='\n${PINK}\u${D} @ ${ORANGE}\h ${D}in ${GREEN}\w\
    ${D}\n$(prompt_char) '
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


