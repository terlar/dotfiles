function chat
  if test -z "$chat_server"
    echo 'error: $chat_server not set'
    return 1
  end

  set -l commands "ssh $chat_server -t t"

  if test -z "$TMUX"
    tmux new -A -s chat "$commands; $SHELL -l"
  else
    set -l sessions (tmux list-sessions | cut -d ':' -f 1 ^/dev/null)

    if not contains -- chat $sessions
      tmux if true "new -d -s chat '$commands; $SHELL -l'"
    end

    tmux switch -t chat
  end
end
