# Automatic URL quotation
autoload -U url-quote-magic
zle -N self-insert url-quote-magic

# Edit command line
autoload -U edit-command-line
zle -N edit-command-line
bindkey -M vicmd v edit-command-line

# Reset prompt on input change
zle-line-init zle-keymap-select() {
  zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select

# Enable vi mode
bindkey -v
# Fn+Delete for delete char
bindkey "^[[3~" delete-char
# Shift+Tab for reverse menu completion
bindkey '^[[Z' reverse-menu-complete

setopt correct

setopt combining_chars
setopt no_flow_control

# Directories
setopt auto_cd
setopt cdable_vars
setopt auto_name_dirs
setopt auto_pushd
setopt pushd_ignore_dups

# Jobs
setopt long_list_jobs
