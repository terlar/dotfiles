t() {
  # Wrapper command for OSX clipboard
  if (( $+commands[reattach-to-user-namespace] )); then
    local wrapper="reattach-to-user-namespace -l $SHELL"
  fi

  # Attatch or open new tmux session
  if (( $# == 0 )); then
    tmux attach || tmux new "$wrapper"
  else
    tmux attach -t $1 || tmux new -s $1 "$wrapper"
  fi
}

# Set tmux window name
_tmux-set-window-name() {
  [ ! -n "$TMUX" ] && return

  local project_path=${PWD##$PROJECTS/}
  if [ "$project_path" != "$PWD" ]; then
    local project_name=${project_path%%/*}
    tmux rename-window $project_name
  fi
}
chpwd_functions+=(_tmux-set-window-name)
