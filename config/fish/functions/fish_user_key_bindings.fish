function fish_user_key_bindings
	set -g fish_key_bindings fish_vi_key_bindings
	fish_default_key_bindings -M insert

	if type -q fzf_key_bindings
		fzf_key_bindings
	end

	#
	# Normal mode
	#
	bind \n '__commandline_execute'
	bind e forward-word backward-char
	bind E forward-bigword backward-char

	bind \ei __commandline_edit
	bind \cl '__commandline_clear_prompt'

	#
	# Insert mode
	#
	bind -M insert --key btab complete-and-search

	bind -M insert \cl '__commandline_clear_prompt'
	bind -M insert \el '__fish_list_current_token; __commandline_reload_prompt'
	bind -M insert \e'<' 'prevd; __commandline_reload_prompt'
	bind -M insert \e'>' 'nextd; __commandline_reload_prompt'

	# Execute
	bind -M insert \n '__commandline_execute'
	bind -M insert \e\n '__commandline_execute_and_keep_line'
	bind -M insert \e',' 'commandline -f execute history-search-backward'
	bind -M insert \em 'commandline -f execute accept-autosuggestion'
	bind -M insert \ez 'fg >/dev/null ^/dev/null'

	# Insert last argument of previous command
	bind -M insert \e. history-token-search-backward

	# Commandline
	bind -M insert \cx __commandline_eval_token
	bind -M insert \er __commandline_sudo_toggle

	# Stash/pop
	bind -M insert \es __commandline_stash
	bind -M insert \eS __commandline_pop

	bind -M insert \cv __commandline_paste
end

function __commandline_execute
	set value (commandline)
	if test -n "$value"
		commandline -f execute
	else
		echo
	end
end

function __commandline_reload_prompt
	set_color normal
	fish_prompt
	commandline -f repaint
end

function __commandline_clear_prompt
	set -ge __prompt_context_current
	clear
	__commandline_reload_prompt
end

function __commandline_insert_previous_token
	set tokens (commandline -po)
	test $tokens[1]; or return

	set previous_token $tokens[-1]

	if test -n (commandline -pt)
		set previous_token " $previous_token"
	end

	commandline -i $previous_token
end

function __commandline_eval_token
	set token (commandline -t)

	if test -n "$token"
		set value (eval string escape $token | string join ' ')
		if test -n "$value" -a "$value" != ' '
			commandline -t $value
			commandline -f backward-char
		end
	end
end

function __commandline_edit --description 'Input command in external editor'
	set f (mktemp /tmp/fish.cmd.XXXXXXXX)
	if test -n "$f"
		set p (commandline -C)
		commandline > $f
		eval $EDITOR $f
		commandline (more $f)
		commandline -C $p
		command rm $f
	end
end

function __commandline_sudo_toggle
	set pos (commandline -C)
	set cmd (commandline)

	if string match -q 'sudo *' $cmd
		set pos (expr $pos - 5)
		commandline (string replace 'sudo ' '' $cmd)
	else
		commandline -C 0
		commandline -i 'sudo '
		set pos (expr $pos + 5)
	end

	commandline -C $pos
end

function __commandline_stash -d 'Stash current command line'
	set cmd (commandline)
	test "$cmd"; or return
	set pos (commandline -C)

	set -U command_stash $command_stash $cmd
	set -U __command_stash_pos $__command_stash_pos $pos
	commandline ''
end

function __commandline_pop -d 'Pop last stashed command line'
	if not set -q command_stash[-1]
		return
	end

	commandline $command_stash[-1]

	if set -q __command_stash_pos[-1]
		commandline -C $__command_stash_pos[-1]
		set -e __command_stash_pos[-1]
	end

	set -e command_stash[-1]
end

function __commandline_toggle -d 'Stash current commandline if not empty, otherwise pop last stashed commandline'
	set cmd (commandline)

	if test "$cmd"
		__commandline_stash
	else
		__commandline_pop
	end
end

function __commandline_paste
	if type -fq pbpaste
		commandline -i (pbpaste)
		return
	end

	if not set -qg DISPLAY
		return
	end

	if type -fq xsel
		commandline -i (xsel)
	else if type -fq xclip
		commandline -i (xclip -o)
	end
end

function __commandline_sudo_execute
	commandline -C 0
	commandline -i 'sudo '
	commandline -f execute
end

function __commandline_execute_and_keep_line
	set pos (commandline -C)
	set cmd (commandline)

	commandline -f execute

	while true
		set funcname __fish_restore_line_(random);
		if not functions $funcname >/dev/null ^/dev/null
			break;
		end
	end

	function $funcname -V funcname -V pos -V cmd -j %self
		commandline "$cmd"
		commandline -C $pos
		functions -e $funcname
	end
end
