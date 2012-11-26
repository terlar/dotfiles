function t --description 'Attach or open new tmux session'
  # Wrapper command for OSX clipboard
  set -l wrapper
  if which reattach-to-user-namespace ^/dev/null
    set wrapper "reattach-to-user-namespace -l $SHELL"
  end

  if test (count $argv) -gt 0
    tmux attach -t $argv[1]
    or tmux new -s $argv[1] $wrapper
  else
    tmux attach
    or tmux new $wrapper
  end
end
