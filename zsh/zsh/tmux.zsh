# Set tmux window name
_tmux-set-window-name() {
  [ ! -n "$TMUX" ] && return

  if [ "${PWD##$PROJECTS/}" != "$PWD" ]; then
    tmux rename-window ${PWD##*/}
  fi
}
chpwd_functions+=(_tmux-set-window-name)
