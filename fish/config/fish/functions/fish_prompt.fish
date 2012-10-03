function fish_prompt --description 'Write out the prompt'
  set -l last_status $status

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

  if test $last_status -eq 0
    set_color normal
  else
    set_color $fish_color_error
  end

  printf ' ➤ '
  set_color normal
end
