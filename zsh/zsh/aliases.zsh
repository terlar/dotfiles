alias reload!='. ~/.zshrc'

alias ls="ls -hF"
alias mkdir="mkdir -p"
alias grep="grep --color"
# copy with a progress bar
alias cpv="rsync -poghb --backup-dir=/tmp/rsync -e /dev/null --progress --"

# Ruby/Rails
alias rc="rails c"
alias bundle='nocorrect bundle'
