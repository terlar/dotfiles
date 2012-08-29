# Reload zsh config
alias reload!='. ~/.zshrc'

# Super user
alias _='sudo'

# File system
ls --color -d . &>/dev/null 2>&1 && alias lscolor='ls --color=tty' || alias lscolor='ls -G'
alias ls="lscolor -hF"
alias mkdir="mkdir -p"
# Copy with a progress bar
alias cpv="rsync -poghb --backup-dir=/tmp/rsync -e /dev/null --progress --"

# Git
alias g='git'
compdef g=git
alias gst='git status'
compdef _git gst=git-status
alias gd='git diff'
compdef _git gd=git-diff
gdv() { git diff -w "$@" | view - }
compdef _git gdv=git-diff
alias gc='git commit -v'
compdef _git gc=git-commit
alias gca='git commit -v -a'
compdef _git gca=git-commit
alias gco='git checkout'
compdef _git gco=git-checkout
alias ga='git add'
compdef _git ga=git-add

# Ruby/Rails
alias rc="rails c"
