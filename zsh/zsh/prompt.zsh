# Get current branch name
function _git_prompt_info() {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  echo "$GIT_PROMPT_PREFIX${ref#refs/heads/}$(_parse_git_dirty)$GIT_PROMPT_SUFFIX"
}
# Checks if working tree is dirty
function _parse_git_dirty() {
  if [[ -n $(git status -s 2> /dev/null) ]]; then
    echo "$GIT_PROMPT_DIRTY"
  else
    echo "$GIT_PROMPT_CLEAN"
  fi
}
# Get the status of the working tree
function _git_prompt_status() {
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

function _vimode_prompt_info() {
  echo "${${KEYMAP/vicmd/$VIMODE_PROMPT_NORMAL}/(main|viins)/$VIMODE_PROMPT_INSERT}"
}
function _status_result() {
  echo "%(?,$STATUS_RESULT_PROMPT_SUCCESS,$STATUS_RESULT_PROMPT_FAIL)"
}
function _prompt_char() {
  if [[ $KEYMAP == '' ]]; then
    _status_result
  else
    _vimode_prompt_info
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

VIMODE_PROMPT_NORMAL="%{$fg_bold[cyan]%}➤%{$reset_color%}"
VIMODE_PROMPT_INSERT="➤"

STATUS_RESULT_PROMPT_SUCCESS="%{$fg_bold[green]%}➤%{$reset_color%}"
STATUS_RESULT_PROMPT_FAIL="%{$fg_bold[red]%}➤%{$reset_color%}"

PROMPT='%{$fg[magenta]%}%n%{$reset_color%}@%{$fg[yellow]%}%m%{$reset_color%}:%{$fg[cyan]%}%0~%{$reset_color%}$(_git_prompt_info)$(_git_prompt_status)
$(_prompt_char) '
