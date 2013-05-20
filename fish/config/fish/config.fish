set fish_greeting

set -x TERM xterm-256color
set -x EDITOR vim
set -x LC_ALL en_US.UTF-8

# Less colors
set -x LESS_TERMCAP_mb \e'[01;31m'       # begin blinking
set -x LESS_TERMCAP_md \e'[01;38;5;74m'  # begin bold
set -x LESS_TERMCAP_me \e'[0m'           # end mode
set -x LESS_TERMCAP_so \e'[38;5;246m'    # begin standout-mode - info box
set -x LESS_TERMCAP_se \e'[0m'           # end standout-mode
set -x LESS_TERMCAP_us \e'[04;38;5;146m' # begin underline
set -x LESS_TERMCAP_ue \e'[0m'           # end underline

# Paths
set PATH $HOME/.local/bin /usr/local/bin $PATH
set -x NODE_PATH /usr/local/lib/node_modules


# Plugins
set -l plugins_path (dirname (status -f))/plugins
# Bundler
. $plugins_path/bundler/bundler.fish

# Farm
. /usr/local/share/fish-farm/farm.fish
# Fry
. /usr/local/share/fry/fry.fish


# Abbreviations
set -q fish_abbreviations; or fish_user_abbreviations
