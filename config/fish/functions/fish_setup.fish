function fish_setup --description 'Setup fish variables'
  set -U fish_greeting

  set -Ux LANG en_US.UTF-8
  set -Ux LC_ALL en_US.UTF-8

  set -Ux EDITOR vim
  set -Ux BROWSER open

  # Less colors
  set -Ux LESS_TERMCAP_mb \e'[01;31m'       # begin blinking
  set -Ux LESS_TERMCAP_md \e'[01;38;5;74m'  # begin bold
  set -Ux LESS_TERMCAP_so \e'[38;5;246m'    # begin standout-mode - info box
  set -Ux LESS_TERMCAP_us \e'[04;38;5;146m' # begin underline
  set -Ux LESS_TERMCAP_me \e'[0m'           # end mode
  set -Ux LESS_TERMCAP_se \e'[0m'           # end standout-mode
  set -Ux LESS_TERMCAP_ue \e'[0m'           # end underline

  # Syntax highligthing colors
  set -U fish_color_command blue
  set -U fish_color_param purple

  # Pager colors
  set -U fish_pager_color_description white

  # Abbreviations
  fish_user_abbreviations

  # Settings
  set -U tank_reporter spec
  set -U fry_auto_switch 1

  set -U fish_setup_done 1
  echo 'Initial fish setup done!'
end
