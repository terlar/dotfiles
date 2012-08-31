HISTFILE=$HOME/.zhistory
HISTSIZE=10000
SAVEHIST=10000

setopt extended_history
setopt inc_append_history
setopt share_history

setopt hist_expire_dups_first
setopt hist_ignore_dups
setopt hist_ignore_space
setopt hist_reduce_blanks
setopt hist_verify
