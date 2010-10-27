# Bootstrap {
    # Load profile
    if [ -f ~/.profile ]; then
        . ~/.profile
    fi
# }

# Prompt {
    setopt prompt_subst
    autoload colors zsh/terminfo

    if [[ "$terminfo[colors]" -gt 7 ]]; then
        colors
    fi

    PROMPT='
%{$fg[magenta]%}%n%{$reset_color%} at %{$fg[yellow]%}%m%{$reset_color%} in %{$fg_bold[green]%}%~%{$reset_color%}
$(prompt_char) '
# }

# Input {
    autoload -U compinit promptinit
    compinit
    promptinit

    zstyle ':completion::complete:*' use-cache 1
    zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
    zstyle ':completion:*' list-colors "=(#b) #([0-9]#)*=36=31"

    zstyle -e ':completion::*:hosts' hosts 'reply=($(sed -e "/^#/d" -e "s/ .*\$//" -e "s/,/ /g" /etc/ssh_known_hosts(N) ~/.ssh/known_hosts(N) 2>/dev/null | xargs) $(grep \^Host ~/.ssh/config(N) | cut -f2 -d\  2>/dev/null | xargs))'
# }

# History {
    export HISTCONTROL=erasedups

    setopt incappendhistory \
    extendedhistory \
    histfindnodups \
    histreduceblanks \
    histignorealldups \
    histsavenodups
# }
