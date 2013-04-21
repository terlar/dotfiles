function fish_user_key_bindings
  # Insert last argument of previous command
  bind \e. history-token-search-backward
  bind \e, __insert-previous-token

  bind \e! __runsudo
end

function __runsudo --description 'Run current command line as root'
  commandline -C 0
  commandline -i 'sudo '
  commandline -f execute
end

function __insert-previous-token
  set -l tokens (commandline -po)
  test $tokens[1]; or return

  set -l previous_token $tokens[-1]

  if test -n (commandline -pt)
    set previous_token " $previous_token"
  end

  commandline -i $previous_token
end
