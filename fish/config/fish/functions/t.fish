function t --description 'Attach or open new tmux session'
  # Wrapper command for OSX clipboard
  if which reattach-to-user-namespace > /dev/null 2> /dev/null
    set -l wrapper (reattach-to-user-namespace -l $SHELL)
  end

  if test (count $argv) = 0
    echo 'count'
    tmux attach; or tmux new $wrapper
  else
    echo 'test'
    tmux attach -t $argv[1]; or tmux new -s $argv[1] $wrapper
  end
end
