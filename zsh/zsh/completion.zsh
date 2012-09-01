zmodload -i zsh/complist
autoload -U compinit; compinit -i;

setopt glob_complete
setopt no_case_glob
setopt numeric_glob_sort
setopt no_list_ambiguous
setopt complete_in_word
setopt always_to_end

# Shift+Tab for reverse menu completion
bindkey '^[[Z' reverse-menu-complete

# Automatic URL quotation
autoload -U url-quote-magic
zle -N self-insert url-quote-magic
# Rationalise dots
zle -N _rationalise-dot
bindkey . _rationalise-dot
# Completion waiting dots
zle -N _expand-or-complete-with-dots
bindkey "^I" _expand-or-complete-with-dots

# Instant completion for single matches
zstyle '*' single-ignored complete
# Use menu selection
zstyle ':completion:*:default' menu select
# Colors
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
# Cache
zstyle ':completion:*' use-cache on

# Enhanced completion with approximate
zstyle ':completion:*' completer _expand _force-rehash _complete _approximate _ignored
zstyle ':completion:*:approximate:*' max-errors 'reply=( $(( ($#PREFIX+$#SUFFIX)/3 )) )'
# Case-insensitive
zstyle ':completion:*' matcher-list 'm:{[:lower:]}={[:upper:]}'

# Group matches
zstyle ':completion:*' group-name ''
zstyle ':completion:*:descriptions' format '%B%d%b'
# Show all matches
zstyle ':completion:*:default' list-prompt '%S%M matches%s'

# Directories
zstyle ':completion:*:cd:*' tag-order local-directories directory-stack path-directories
zstyle ':completion:*' special-dirs true
zstyle ':completion:*' ignore-parents parent pwd

# Processes
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
zstyle ':completion:*:*:*:*:processes' command "ps -u `whoami` -o pid,user,comm -w -w"

# Functions
zstyle ':completion:*:functions' ignored-patterns '_*'
# Users
zstyle ':completion:*:*:*:users' ignored-patterns '_*'
