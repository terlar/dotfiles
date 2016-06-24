#!/usr/bin/env sh
eval "$("$HOME/.env" | sed 's/^/export /g')"

# Input Manager
export GTK_IM_MODULE=ibus
export XMODIFIERS='@im=ibus'
export QT_IM_MODULE=ibus

# QT
export QT_STYLE_OVERRIDE=GTK+

if [ -f "$HOME/.Xresources" ]; then
	xrdb -merge "$HOME/.Xresources"
fi

# Set Root Window Settings
xsetroot -cursor_name left_ptr
# Set DPI
xrandr --dpi 114

systemctl --user import-environment
systemctl --user start graphical.target

/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
gpg-connect-agent /bye
