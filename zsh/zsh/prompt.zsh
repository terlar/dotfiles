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

GIT_PROMPT_PREFIX="|%{$fg[green]%}"
GIT_PROMPT_SUFFIX="%{$reset_color%}"
GIT_PROMPT_DIRTY="%{$fg_bold[red]%}⚡%{$reset_color%}"
GIT_PROMPT_AHEAD="%{$fg_bold[red]%}!%{$reset_color%}"
GIT_PROMPT_CLEAN="%{$fg_bold[green]%}✓%{$reset_color%}"

GIT_PROMPT_ADDED="%{$fg[green]%}✚%{$reset_color%}"
GIT_PROMPT_MODIFIED="%{$fg[blue]%}✹%{$reset_color%}"
GIT_PROMPT_DELETED="%{$fg[red]%}✖%{$reset_color%}"
GIT_PROMPT_RENAMED="%{$fg[magenta]%}➜%{$reset_color%}"
GIT_PROMPT_UNMERGED="%{$fg[yellow]%}═%{$reset_color%}"
GIT_PROMPT_UNTRACKED="%{$fg[cyan]%}✭%{$reset_color%}"

KEYMAP_PROMPT_VICMD="%{$fg_bold[cyan]%}➤%{$reset_color%}"
KEYMAP_PROMPT_VIINS="➤"

EXIT_CODE_PROMPT_SUCCESS="%{$fg_bold[green]%}➤%{$reset_color%}"
EXIT_CODE_PROMPT_FAIL="%{$fg_bold[red]%}➤%{$reset_color%}"

BUNDLE_GEMFILE_PROMPT_PREFIX="|"
BUNDLE_GEMFILE_PROMPT_SUFFIX=""

PROMPT='%{$fg[magenta]%}%n%{$reset_color%}@%{$fg[yellow]%}%m%{$reset_color%}:%{$fg[cyan]%}%0~%{$reset_color%}$(_git_prompt_info)$(_git_prompt_status)$(_bundle_gemfile_prompt)
$(_prompt_char) '
