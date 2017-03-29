#!/usr/bin/env bash
[[ "${TRACE}" ]] && set -x
set -eu
set -o pipefail

export $("$HOME/.env")

# Input Manager
export GTK_IM_MODULE=ibus
export XMODIFIERS='@im=ibus'
export QT_IM_MODULE=ibus

# QT
export QT_STYLE_OVERRIDE=GTK+


# Java GUI
export _JAVA_OPTIONS="\
	-Dawt.useSystemAAFontSettings=on\
	-Dswing.aatext=true\
	-Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel\
	-Dswing.crossplatformlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel"

export _JAVA_AWT_WM_NONREPARENTING=1

if [ -f "$HOME/.Xresources" ]; then
	xrdb -merge "$HOME/.Xresources"
fi

systemctl --user import-environment
systemctl --user start graphical.target

gpg-connect-agent /bye
