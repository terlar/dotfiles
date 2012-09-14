# Reload zsh config
alias reload!='. ~/.zshrc'

# Super user
alias _='sudo'

# File system
ls --color -d . &>/dev/null 2>&1 && alias lscolor='ls --color=tty' || alias lscolor='ls -G'
alias ls='lscolor -hF'
alias l='ls -la'
alias l.='ls -d .*'
alias ll='ls -l'

alias mkdir='mkdir -p'
# Copy with a progress bar
alias cpv='rsync -poghb --backup-dir=/tmp/rsync -e /dev/null --progress --'

# Pipes
alias -g A='| ack'
alias -g C='| wc -l'
alias -g G='| grep'
alias -g H='| head'
alias -g L='| less'
alias -g N='| /dev/null'
alias -g S='| sort'
alias -g T='| tail'
alias -g X='| xargs'
alias -g V='| vim -R -'

# Git
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
# Branch publish
gbp() {
  local remote=$(git remote)
  if [ $# -eq 0 ]; then
    local branch=$(git rev-parse --abbrev-ref HEAD)
  else
    local branch=$1
  fi

  git push "$remote" "$branch" &&
  git branch --set-upstream "$branch" "$remote/$branch"
}
# Branch delete
gbd() {
  local remote=$(git remote)
  if [ $# -eq 0 ]; then
    local branch=$(git rev-parse --abbrev-ref HEAD)
  else
    local branch=$1
  fi

  git branch -r -d "$branch"
  git push "$remote" ":$branch"
}

alias gc='git commit -v'
alias gca='git commit -v -a'

# Ruby/Rails
alias rc='rails c'
