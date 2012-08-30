# ZLE
bindkey -v
setopt combining_chars

# Input
setopt correct
setopt no_flow_control

# Automatic URL quotation
autoload -U url-quote-magic
zle -N self-insert url-quote-magic

# Edit command line
autoload -U edit-command-line
zle -N edit-command-line
bindkey -M vicmd v edit-command-line

# Fn+Backspace for delete char
bindkey "^[[3~" delete-char

# CD
setopt auto_cd
setopt cdable_vars
setopt auto_name_dirs
setopt auto_pushd
setopt pushd_ignore_dups

# Job Control
setopt long_list_jobs
