set -U fish_color_user magenta
set -U fish_color_host yellow

function fish_prompt --description 'Write out the prompt'
  set -l last_status $status

  # User
  set_color $fish_color_user
  printf (whoami)
  set_color normal

  printf '@'

  # Host
  set_color $fish_color_host
  printf (hostname -s)
  set_color normal

  printf ':'

  # PWD
  set_color $fish_color_cwd
  printf (prompt_pwd)
  set_color normal

  fish_prompt_git
  fish_prompt_misc

  if not test $last_status -eq 0
    set_color $fish_color_error
  end

  printf ' ➤ '
  set_color normal
end
