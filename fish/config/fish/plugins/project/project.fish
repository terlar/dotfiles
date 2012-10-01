# Paths
set -xg PROJECTS $HOME/Code
set -xg CDPATH $CDPATH $PROJECTS

# Alias
function p ; project_switch $argv ; end

function __project_tmux_window_name --on-variable PWD --description 'Set tmux window name to project name'
  status --is-command-substitution; and return
  test -z $TMUX; and return

  set -l project_path (echo $PWD | sed 's|^'$PROJECTS'/||')
  test "$project_path" = "$PWD"; and return

  set -l project_name (echo $project_path | cut -f1 -d '/')
  tmux rename-window $project_name
end
