#!/bin/sh

# Display settings
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources
xset +fp /usr/local/share/fonts/local
xset fp rehash
xsetroot -cursor_name left_ptr
compton -fb --backend glx &

# Input settings
xset r rate 220 45
xmodmap ~/.Xmodmap
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS="@im=fcitx"
# fcitx &

# SSH Settings
eval $(/usr/bin/ssh-agent)

# Desktop settings
feh --bg-fill ~/Pictures/dark-linnen.png
termite &

export PATH=$PATH:$HOME/.cabal/bin
xmonad
