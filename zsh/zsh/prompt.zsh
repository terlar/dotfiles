_keymap_prompt() {
  case $KEYMAP in
    "vicmd")
      echo $KEYMAP_PROMPT_VICMD;;
    "main"|"viins")
      echo $KEYMAP_PROMPT_VIINS;;
  esac
}

_exit_code_prompt() {
  echo "%(?,$EXIT_CODE_PROMPT_SUCCESS,$EXIT_CODE_PROMPT_FAIL)"
}

_prompt_char() {
  if [ $KEYMAP ]; then
    _keymap_prompt
  else
    _exit_code_prompt
  fi
}

# Reset prompt on input change
zle-line-init zle-keymap-select() {
  zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select

setopt prompt_subst

KEYMAP_PROMPT_VICMD="%F{cyan}➤%f"
KEYMAP_PROMPT_VIINS="➤"

EXIT_CODE_PROMPT_SUCCESS="%F{green}➤%f"
EXIT_CODE_PROMPT_FAIL="%F{red}➤%f"

BUNDLE_GEMFILE_PROMPT_PREFIX="|"
BUNDLE_GEMFILE_PROMPT_SUFFIX=""

PROMPT='%F{magenta}%n%f@%F{yellow}%m%f:%F{cyan}%0~%f${vcs_info_msg_0_}$(_bundle_gemfile_prompt)
$(_prompt_char) '
