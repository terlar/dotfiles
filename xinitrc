#!/bin/sh

# Display settings
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources
xsetroot -cursor_name left_ptr

# Input settings
xset r rate 220 45
xmodmap ~/.Xmodmap

# SSH Settings
eval $(ssh-agent)

# Audio
source /etc/X11/xinit/xinitrc.d/pulseaudio

# Desktop settings
feh --bg-tile ~/Pictures/linnen.png
termite &

export PATH=$PATH:$HOME/.cabal/bin
xmonad
