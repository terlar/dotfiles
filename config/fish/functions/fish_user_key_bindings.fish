function fish_user_key_bindings
	bind \el '__fish_list_current_token; echo'
	bind \e'<' 'prevd; set -ge __prompt_context_current; fish_prompt'
	bind \e'>' 'nextd; set -ge __prompt_context_current; fish_prompt'
	bind \cl 'set -ge __prompt_context_current; clear; set_color normal; fish_prompt; commandline -f repaint'

	# Insert last argument of previous command
	bind \e. history-token-search-backward
	bind \e, __commandline_insert_previous_token

	bind \cx __commandline_eval_token
	bind \ee __commandline_edit
	bind \er __commandline_sudo_toggle

	# Stash/pop
	bind \es __commandline_stash
	bind \eS __commandline_pop

	# Execute
	bind \e! __commandline_sudo_execute
	bind \em 'commandline -f execute accept-autosuggestion'
	bind \ez 'commandline "fg"; commandline -f execute'
end

function __commandline_insert_previous_token
	set -l tokens (commandline -po)
	test $tokens[1]; or return

	set -l previous_token $tokens[-1]

	if test -n (commandline -pt)
		set previous_token " $previous_token"
	end

	commandline -i $previous_token
end

function __commandline_eval_token
	set -l token (commandline -t)

	if test -n "$token"
		set -l value (eval __fish_list $token | tr \n ' ')
		if test -n "$value" -a "$value" != ' '
			commandline -t $value
			commandline -f backward-char
		end
	end
end

function __commandline_edit --description 'Input command in external editor'
	set -l f (mktemp /tmp/fish.cmd.XXXXXXXX)
	if test -n "$f"
		set -l p (commandline -C)
		commandline -b > $f
		vim -c 'set ft=fish' $f
		commandline -r (more $f)
		commandline -C $p
		command rm $f
	end
end

function __commandline_sudo_toggle
	set -l pos (commandline -C)
	set -l cmd (commandline -b)

	if echo $cmd | grep '^sudo ' >/dev/null
		set pos (math $pos - 5)
		commandline (echo $cmd | cut -c6- )
	else
		commandline -C 0
		commandline -i 'sudo '
		set pos (math $pos + 5)
	end

	commandline -C $pos
end

function __commandline_stash -d 'Stash current command line'
	set -g __stash_command_position (commandline -C)
	set -g __stash_command (commandline -b)
	commandline -r ''
end

function __commandline_pop -d 'Pop last stashed command line'
	if not set -q __stash_command
		return
	end

	commandline -r $__stash_command

	if set -q __stash_command_position
		commandline -C $__stash_command_position
	end

	set -e __stash_command
	set -e __stash_command_position
end

function __commandline_toggle -d 'Stash current commandline if not empty, otherwise pop last stashed commandline'
	set -l cmd (commandline -b)

	if test "$cmd"
		__commandline_stash
	else
		__commandline_pop
	end
end

function __commandline_sudo_execute
	commandline -C 0
	commandline -i 'sudo '
	commandline -f execute
end

function __commandline_execute_and_keep_line
	set -l pos (commandline -C)
	set -l cmd (commandline -b)

	commandline -f execute
	commandline -r "$cmd"
	commandline -C $pos
end

function __fish_list
	for item in $argv
		echo $item | sed -e 's/ /\\\\ /g'
	end
end
