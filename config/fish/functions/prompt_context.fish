function prompt_context
	set prompt ' '
	set git_dir (command git rev-parse --show-toplevel ^/dev/null)

	set strong_color (set_color black --bold)
	set normal_color (set_color normal)
	set subtle_color (set_color white)

	if set -q TMUX
		set strong_color '#[fg=black,bold]'
		set normal_color '#[default]'
		set subtle_color '#[fg=white]'
	end


	if test -n "$git_dir"
		set project (string split '/' $git_dir)[-1]
		set short_pwd (string replace $git_dir $project $PWD)

		set branch (command git symbolic-ref --short --quiet HEAD)
		set dirty (command git diff --no-ext-diff --ignore-submodules --quiet --exit-code; echo $status)
		set unmerged (command git cherry -v @\{upstream\} ^/dev/null)

		if test $dirty -ne 0
			set branch {$subtle_color}{$branch}
		else
			set branch {$strong_color}{$branch}
		end

		if test -n "$unmerged"
			set branch $branch'⟳'
		end

		set prompt {$prompt}{$strong_color}{$short_pwd}{$normal_color}' on '{$branch}{$normal_color}
	else
		set short_pwd (string replace -r "^$HOME" '~' $PWD)
		set prompt {$prompt}{$strong_color}{$short_pwd}{$normal_color}
	end

	if set -q TMUX
		tmux set -q status-left $prompt
	else
		echo $prompt
	end
end
