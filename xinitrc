#!/bin/sh

xsetroot -cursor_name left_ptr &
xrdb -merge ~/.xresources &
xmodmap ~/.xmodmap &

eval $(/usr/bin/ssh-agent)

unclutter &
termite &

exec xmonad
