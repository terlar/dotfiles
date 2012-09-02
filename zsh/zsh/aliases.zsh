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
alias -g H='| head'
alias -g L='| less'
alias -g N='| /dev/null'
alias -g S='| sort'
alias -g X='| xargs'

# Ruby/Rails
alias rc='rails c'
