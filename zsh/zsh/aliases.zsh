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

# Ruby/Rails
alias rc="rails c"
alias localgem_on='export BUNDLE_GEMFILE=Gemfile.local'
alias localgem_off='unset BUNDLE_GEMFILE'
