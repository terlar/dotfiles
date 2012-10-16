function fish_prompt --description 'Write out the prompt'
  set -l last_status $status

  # User
  set_color $fish_color_user
  printf (whoami)
  set_color normal

  echo -n '@'

  # Host
  set_color $fish_color_host
  printf (hostname -s)
  set_color normal

  echo -n ':'

  # PWD
  set_color $fish_color_cwd
  printf (prompt_pwd)
  set_color normal

  __terlar_git_prompt
  __terlar_misc_prompt
  echo

  if not test $last_status -eq 0
    set_color $fish_color_error
  end

  echo -n '➤ '
  set_color normal
end
