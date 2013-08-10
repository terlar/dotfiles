function fish_right_prompt --description 'Write out the prompt'
  if set -q CMD_DURATION
    set_color white
    echo -n "($CMD_DURATION) "
    set_color normal
  end
end
