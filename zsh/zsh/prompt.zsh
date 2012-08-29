# Get current branch name
_git_prompt_info() {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  echo "$GIT_PROMPT_PREFIX${ref#refs/heads/}$(_parse_git_dirty)$GIT_PROMPT_SUFFIX"
}
# Checks if working tree is dirty
_parse_git_dirty() {
  if [[ -n $(git status -s 2> /dev/null) ]]; then
    echo "$GIT_PROMPT_DIRTY"
  else
    echo "$GIT_PROMPT_CLEAN"
  fi
}
# Get the status of the working tree
_git_prompt_status() {
  INDEX=$(git status --porcelain 2> /dev/null)
  STATUS=""
  if $(echo "$INDEX" | grep '^?? ' &> /dev/null); then
    STATUS="$GIT_PROMPT_UNTRACKED$STATUS"
  fi
  if $(echo "$INDEX" | grep '^A  ' &> /dev/null); then
    STATUS="$GIT_PROMPT_ADDED$STATUS"
  elif $(echo "$INDEX" | grep '^M  ' &> /dev/null); then
    STATUS="$GIT_PROMPT_ADDED$STATUS"
  fi
  if $(echo "$INDEX" | grep '^ M ' &> /dev/null); then
    STATUS="$GIT_PROMPT_MODIFIED$STATUS"
  elif $(echo "$INDEX" | grep '^AM ' &> /dev/null); then
    STATUS="$GIT_PROMPT_MODIFIED$STATUS"
  elif $(echo "$INDEX" | grep '^ T ' &> /dev/null); then
    STATUS="$GIT_PROMPT_MODIFIED$STATUS"
  fi
  if $(echo "$INDEX" | grep '^R  ' &> /dev/null); then
    STATUS="$GIT_PROMPT_RENAMED$STATUS"
  fi
  if $(echo "$INDEX" | grep '^ D ' &> /dev/null); then
    STATUS="$GIT_PROMPT_DELETED$STATUS"
  elif $(echo "$INDEX" | grep '^AD ' &> /dev/null); then
    STATUS="$GIT_PROMPT_DELETED$STATUS"
  fi
  if $(echo "$INDEX" | grep '^UU ' &> /dev/null); then
    STATUS="$GIT_PROMPT_UNMERGED$STATUS"
  fi
  echo $STATUS
}

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

_bundle_gemfile_prompt() {
  if [ $BUNDLE_GEMFILE ]; then
    echo "$BUNDLE_GEMFILE_PROMPT_PREFIX${BUNDLE_GEMFILE##*/}$BUNDLE_GEMFILE_PROMPT_SUFFIX"
  fi
}

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
