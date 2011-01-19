# Load profile
[[ -f ~/.profile ]] && .  ~/.profile

# Title
case $TERM in
	xterm*)
		precmd () {print -Pn "\e]0;%n@%m: %~\a"}
		;;
esac

# Prompt
setopt prompt_subst
autoload colors zsh/terminfo
colors

PROMPT='
%{$fg[magenta]%}%n%{$reset_color%} @ %{$fg[yellow]%}%m%{$reset_color%} : %{$fg_bold[green]%}%~%{$reset_color%}
$(prompt_char) '

# Input
bindkey -v
bindkey '^?' backward-delete-char

# Completion
autoload -U compinit
compinit

zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
zstyle ':completion:*' list-colors "=(#b) #([0-9]#)*=36=31"
zstyle ':completion:*' special-dirs true

zstyle -e ':completion::*:hosts' hosts 'reply=($(sed -e "/^#/d" -e "s/ .*\$//" -e "s/,/ /g" /etc/ssh_known_hosts(N) ~/.ssh/known_hosts(N) 2>/dev/null | xargs) $(grep \^Host ~/.ssh/config(N) | cut -f2 -d\  2>/dev/null | xargs))'

rationalise-dot() {
	if [[ $LBUFFER = *.. ]]; then
		LBUFFER+=/..
	else
		LBUFFER+=.
	fi
}
zle -N rationalise-dot
bindkey . rationalise-dot


# History
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.zsh/history
setopt append_history
setopt inc_append_history
setopt extended_history
setopt hist_find_no_dups
setopt hist_ignore_all_dups
setopt hist_reduce_blanks
setopt hist_ignore_space
setopt hist_no_store
setopt hist_no_functions
setopt no_hist_beep
setopt hist_save_no_dups
