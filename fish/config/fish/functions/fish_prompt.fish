function fish_prompt --description 'Write out the prompt'
  set -l last_status $status

  echo
  set_color magenta
  printf (whoami)
  set_color normal

  printf '@'
  set_color yellow
  printf (hostname -s)
  set_color normal

  printf ':'
  set_color $fish_color_cwd
  printf (prompt_pwd)
  set_color normal

  misc_prompt
  echo

  if not test $last_status -eq 0
    set_color red
  else
    set_color normal
  end

  printf '➤ '
  set_color normal
end
