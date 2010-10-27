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
    function prompt_char {
        git branch >/dev/null 2>/dev/null && echo '±' && return
        svn info >/dev/null 2>/dev/null && echo '✔' && return
        echo '○'
    }
# }

# Aliases {
    alias ..='cd ..'
    alias ...='cd ../..'
    alias ....='cd ../../..'
    alias .....='cd ../../../..'
    alias ls="ls -h"
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

    # Colors
    export CLICOLOR=1
    export LSCOLORS=ExGxBxDxCxEgEdxbxgxcxd

    # Tab completion ignore pattern
    export FIGNORE=.svn
# }


