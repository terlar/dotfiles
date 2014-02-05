function fish_user_key_bindings
  bind \ez 'commandline "fg"; commandline -f execute'

  # Insert last argument of previous command
  bind \e. history-token-search-backward
  bind \e, __insert-previous-token

  bind \el '__fish_list_current_token; echo'
  bind \e'<' 'prevd; set -ge __prompt_context_current; fish_prompt'
  bind \e'>' 'nextd; set -ge __prompt_context_current; fish_prompt'
  bind \cl 'set -ge __prompt_context_current; clear; set_color normal; fish_prompt; commandline -f repaint'

  bind \e! __runsudo

  # Stash/pop
  bind \es __commandline_stash
  bind \eS __commandline_pop

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

function __commandline_stash -d 'Stash current command line'
  set -g __stash_command_position (commandline -C)
  set -g __stash_command (commandline -b)
  commandline -r ''
end

function __commandline_pop -d 'Pop last stashed command line'
  if not set -q __stash_command
    return
  end

  commandline -r $__stash_command

  if set -q __stash_command_position
    commandline -C $__stash_command_position
  end

  set -e __stash_command
  set -e __stash_command_position
end

function __commandline_toggle -d 'Stash current commandline if not empty, otherwise pop last stashed commandline'
  set -l cmd (commandline -b)

  if test "$cmd"
    __commandline_stash
  else
    __commandline_pop
  end
end
