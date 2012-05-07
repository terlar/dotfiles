export TERM="xterm-256color"

# Locale
export LANG="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"

# Editor
export VISUAL="vim"
export EDITOR=$VISUAL
export SVN_EDITOR=$VISUAL

# Colors
export CLICOLOR=1

# Input
set meta-flag on
set input-meta on
set output-meta on
set convert-meta off

fpath=($HOME/.zsh/functions $fpath)
autoload -U $HOME/.zsh/functions/*(:t)

setopt multibyte
setopt combining_chars
