# Git status
_git_status() {
  cat "/tmp/git-status-$$"
}
_git-status-tmp() {
  $(git status --porcelain 2> /dev/null >! "/tmp/git-status-$$")
}
precmd_functions+=(_git-status-tmp)

# Get current branch name
_git_prompt_info() {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  echo "$GIT_PROMPT_PREFIX${ref#refs/heads/}$(_parse_git_dirty)$GIT_PROMPT_SUFFIX"
}

# Checks if working tree is dirty
_parse_git_dirty() {
  if [[ -n $(_git_status) ]]; then
    echo "$GIT_PROMPT_DIRTY"
  else
    echo "$GIT_PROMPT_CLEAN"
  fi
}

# Get the status of the working tree
_git_prompt_status() {
  INDEX=$(_git_status)
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

# Aliases
alias g='git'

alias ga='git add'
alias gb='git branch'
alias gco='git checkout'
alias gf='git fetch'
alias gm='git merge'
alias gr='git rebase'
alias gs='git status'
alias gd='git diff'
gdv() { git diff -w "$@" | view - }

alias gc='git commit -v'
alias gca='git commit -v -a'
