function fish_prompt --description 'Write out the prompt'
  set -l last_status $status

  function -v PWD __prompt_context_output
    status --is-command-substitution; and return
    set -g LPWD $PWD
    prompt_context
  end

  if test -z $LPWD
    set -g LPWD $PWD
    prompt_context
  end

  if test $last_status -ne 0
    set_color $fish_color_error
  else
    set_color normal
    set_color white
  end

  __fish_prompt_line_divider
  echo -n '➥ '
  set_color normal
end

function __fish_prompt_line_divider
  for i in (seq 2 $COLUMNS)
    echo -n —
  end
  echo
end
