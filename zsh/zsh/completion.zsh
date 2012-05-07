setopt glob_complete
setopt no_case_glob
setopt numeric_glob_sort
unsetopt list_ambiguous

zstyle ':completion:*' matcher-list 'm:{a-zåäö}={A-ZÅÄÖ}'
zstyle ':completion:*' special-dirs true

zstyle ':completion:*' verbose yes
zstyle ':completion:*:descriptions' format '%B%d%b'
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:warnings' format 'No matches for: %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' completer _expand _force_rehash _complete _approximate _ignored

zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*:default' list-prompt '%S%M matches%s'
zstyle ':completion:*:default' menu 'select=0'
zstyle ':completion:*' file-sort modification reverse
zstyle ':completion:*' list-colors "=(#b) #([0-9]#)*=36=31"

zstyle ':completion:*:corrections' format '%B%d (errors %e)%b'
zstyle ':completion:*:approximate:*' max-errors 'reply=(  $((  ($#PREFIX+$#SUFFIX)/3  ))  )'
zstyle ':completion:*' ignore-parents parent pwd
zstyle ':completion::approximate*:*' prefix-needed false

zle -N _rationalise_dot
bindkey . _rationalise_dot
