# Load profile
[[ -f ~/.profile ]] && .  ~/.profile

# Title
case $TERM in
	xterm*)
		PS1="\[\033]0;\u@\h: \w\007\]bash\\$ "
		;;
	*)
		PS1="bash\\$ "
		;;
esac

# Prompt
D=$'\e[37;40m'
PINK=$'\e[35;40m'
GREEN=$'\e[32;40m'
ORANGE=$'\e[33;40m'

export PS1='\n${PINK}\u${D} @ ${ORANGE}\h ${D}in ${GREEN}\w\
${D}\n$(prompt_char) '

# Input
set -o vi
bind "set completion-ignore-case on"
bind "set bell-style none"
bind "set show-all-if-ambiguous on"

# History
shopt -s histappend
export HISTCONTROL=ignoredups
export HISTIGNORE='??'
