# Config {
	# Editor
	export VISUAL='vim'
	export EDITOR=$VISUAL
	export SVN_EDITOR=$VISUAL

	# Colors
	export CLICOLOR=1
	export LSCOLORS=ExGxBxDxCxEgEdxbxgxcxd

	# Tab completion ignore pattern
	export FIGNORE=.svn
	export PATH=/usr/local/bin:$PATH
# }

# Aliases {
	alias ls="ls -h"
# }

# Functions {
	# mkdir, cd into it
	function mkcd () {
		mkdir -p "$*"
		cd "$*"
	}
	# svn diff into vim
	function svndiff () {
		svn diff "$@" | vim -R -
	}
	# get prompt char
	function prompt_char {
		git branch >/dev/null 2>/dev/null && echo '±' && return
		svn info >/dev/null 2>/dev/null && echo '✔' && return
		echo '○'
	}
# }

# Use .localrc for settings specific to one system
[[ -f ~/.localrc ]] && .  ~/.localrc
