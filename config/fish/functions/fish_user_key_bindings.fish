function fish_user_key_bindings
  bind \ez 'commandline "fg"; commandline -f execute'

  # Insert last argument of previous command
  bind \e. history-token-search-backward
  bind \e, __insert-previous-token

  bind \el 'echo; __fish_list_current_token; echo'
  bind \ew 'set tok (commandline -pt); if test $tok[1]; echo; whatis $tok[1]; commandline -f repaint; end; echo'
  bind \e'<' 'prevd; commandline -f repaint'
  bind \e'>' 'nextd; commandline -f repaint'
  bind \cl 'set -e __prompt_context_current; clear; fish_prompt; commandline -f repaint'

  bind \e! __runsudo

  bind \cx __fish_eval_token
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

function __fish_list
  for item in $argv
    echo $item | sed -e 's/ /\\\\ /g'
  end
end

function __fish_eval_token
  set -l token (commandline -t)

  if test -n "$token"
    set -l value (eval __fish_list $token | tr \n ' ')
    if test -n "$value" -a "$value" != ' '
      commandline -t $value
      commandline -f backward-char
    end
  end
end
