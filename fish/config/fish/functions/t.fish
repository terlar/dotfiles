function t --description 'Attach or open new tmux session'
  # Wrapper command for OSX clipboard
  set -l wrapper
  if which reattach-to-user-namespace >/dev/null 2>&1
    set wrapper "reattach-to-user-namespace -l $SHELL"
  end

  if test (count $argv) = 0
    tmux attach; or tmux new $wrapper
  else
    tmux attach -t $argv[1]; or tmux new -s $argv[1] $wrapper
  end
end
