#!/usr/bin/env bash
[[ "${TRACE}" ]] && set -x
set -eu
set -o pipefail

# shellcheck disable=SC2046
export $(ev.sh auto)

# Input Manager
export GTK_IM_MODULE=ibus
export XMODIFIERS='@im=ibus'
export QT_IM_MODULE=ibus

# QT
export QT_STYLE_OVERRIDE=GTK+

# GTK
export GTK2_RC_FILES=/usr/share/themes/Pop-light-compact/gtk-2.0/gtkrc
export GTK_THEME=Pop-light-compact

# Java GUI
export _JAVA_AWT_WM_NONREPARENTING=1

if [ -f "$HOME/.Xresources" ]; then
	xrdb -merge "$HOME/.Xresources"
fi

systemctl --user import-environment
systemctl --user start graphical.target

gpg-connect-agent /bye
