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
