autoload -U vcs_info
precmd_functions+=(vcs_info)

VCS_INFO_CLEAN="%F{green}✓%f"
VCS_INFO_STAGED="%F{yellow}⚡%f"
VCS_INFO_UNSTAGED="%F{red}⚡%f"
VCS_INFO_AHEAD="%F{red}!%f"

VCS_INFO_UNTRACKED="%F{cyan}✭%f"
VCS_INFO_ADDED="%F{green}✚%f"
VCS_INFO_MODIFIED="%F{blue}*%f"
VCS_INFO_DELETED="%F{red}✖%f"
VCS_INFO_RENAMED="%F{magenta}➜%f"
VCS_INFO_UNMERGED="%F{yellow}═%f"

zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:git:*' formats '|%F{green}%b%f%c%u%m'
zstyle ':vcs_info:git:*' check-for-changes true
zstyle ':vcs_info:git:*' stagedstr $VCS_INFO_STAGED
zstyle ':vcs_info:git:*' unstagedstr $VCS_INFO_UNSTAGED

zstyle ':vcs_info:git*+set-message:*' hooks git-status
+vi-git-status() {
  hook_com[misc]+=$(_git_status)
}

_git_status() {
  local gitindex gitstatus
  gitindex=$(git status --porcelain 2> /dev/null)

  if [ -z "$gitindex" ]; then
    echo $VCS_INFO_CLEAN
    return
  fi

  if $(echo "$gitindex" | grep '^?? ' &> /dev/null); then
    gitstatus+=$VCS_INFO_UNTRACKED
  fi
  if $(echo "$gitindex" | grep '^A  \|^M  ' &> /dev/null); then
    gitstatus+=$VCS_INFO_ADDED
  fi
  if $(echo "$gitindex" | grep '^ M \|^AM \|^ T ' &> /dev/null); then
    gitstatus+=$VCS_INFO_MODIFIED
  fi
  if $(echo "$gitindex" | grep '^ D \|^AD ' &> /dev/null); then
    gitstatus+=$VCS_INFO_DELETED
  fi
  if $(echo "$gitindex" | grep '^R  ' &> /dev/null); then
    gitstatus+=$VCS_INFO_RENAMED
  fi
  if $(echo "$gitindex" | grep '^UU ' &> /dev/null); then
    gitstatus+=$VCS_INFO_UNMERGED
  fi

  echo $gitstatus
}

# Aliases
alias g='git'
alias gs='git status'

alias gd='git diff'
gdv() { git diff -w "$@" | view - }
compdef _git gdv=git-diff
alias gwc='git whatchanged -p --abbrev-commit --pretty=medium'

alias ga='git add'
alias gc='git commit -v'
alias gca='git commit -v -a'

alias gb='git branch'
alias gco='git checkout'

alias gf='git fetch'
alias gm='git merge'
alias gr='git rebase'

# Branch publish
gbp() {
  local remote=$(git remote)
  if [ -z $remote ]; then
    return 0
  fi

  if [ $# -eq 0 ]; then
    local branch=$(git rev-parse --abbrev-ref HEAD)
  else
    local branch=$1
  fi

  git push "$remote" "$branch" &&
  git branch --set-upstream "$branch" "$remote/$branch"
}
compdef _git gbp=git-branch

# Branch delete
gbd() {
  local remote=$(git remote)
  if [ -z $remote ]; then
    return 0
  fi
  local current_branch=$(git rev-parse --abbrev-ref HEAD)

  if [ $# -eq 0 ]; then
    local branch=$current_branch
  else
    local branch=$1
  fi

  echo -n "gbd: sure you want to delete local AND remote branch $branch [yn]? "
  read confirm
  if [ "$confirm" != "y" ] && return 0

  if [ $branch == $current_branch ] && git checkout -
  git branch -d "$branch"
  git push "$remote" ":$branch"
}
compdef _git gbd=git-branch
