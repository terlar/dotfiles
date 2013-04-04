function project_switch --description 'Switch project'
  set -l session_name $argv[1]
  set -l projects (ls -1 $PROJECTS | cat)

  if not contains $session_name $projects
    echo 'Available projects:'
    for p in $projects
      echo "  $p"
    end

    return
  end

  set -l command " cd $PROJECTS/$session_name; clear"
  if not which tmux >/dev/null
    eval $command
    return
  end

  set -l sessions (tmux list-sessions | cut -d ':' -f 1 ^/dev/null)

  # Wrapper command for OSX clipboard
  set -l wrapper
  if which reattach-to-user-namespace >/dev/null
    set wrapper "reattach-to-user-namespace -l $SHELL"
  end

  if test -z "$TMUX"
    if not contains $session_name $sessions
      tmux new -d -s $session_name $wrapper
      tmux send-keys -t $session_name $command Enter
    end

    tmux attach -t $session_name
  else
    if not contains $session_name $sessions
      tmux if 'true' "new -d -s $session_name"
      tmux send-keys -t $session_name $command Enter
    end

    tmux switch -t $session_name
  end
end
