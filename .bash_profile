# Bootstrap {
    # Load profile
    if [ -f ~/.profile ]; then
        . ~/.profile
    fi
    # Get the aliases and functions
    if [ -f ~/.bashrc ]; then
        . ~/.bashrc
    fi
# }

# Prompt {
    D=$'\e[37;40m'
    PINK=$'\e[35;40m'
    GREEN=$'\e[32;40m'
    ORANGE=$'\e[33;40m'

    export PS1='\n${PINK}\u${D} @ ${ORANGE}\h ${D}in ${GREEN}\w\
${D}\n$(prompt_char) '
# }

# Input {
    set completion-ignore-case on
# }

# History {
    shopt -s histappend
    export HISTCONTROL=ignoredups
    export HISTIGNORE='??'
# }
