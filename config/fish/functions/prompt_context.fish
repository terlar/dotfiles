function prompt_context
	set -l prompt ' '
	set -l git_dir (command git rev-parse --show-toplevel ^/dev/null)

	set -l strong_color (set_color black --bold)
	set -l normal_color (set_color normal)
	set -l subtle_color (set_color white)

	if set -q TMUX
		set strong_color '#[fg=black,bold]'
		set normal_color '#[default]'
		set subtle_color '#[fg=white]'
	end


	if test -n "$git_dir"
		set short_pwd (echo $PWD | sed -e "s|^$git_dir|"(basename $git_dir)"|")
		set -l branch (command git symbolic-ref --short --quiet HEAD)
		set -l dirty (command git diff --no-ext-diff --ignore-submodules --quiet --exit-code; echo $status)
		set -l unmerged (command git cherry -v @\{upstream\} ^/dev/null)

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
		set -l short_pwd (echo $PWD | sed -e "s|^$HOME|~|")
		set prompt {$prompt}{$strong_color}{$short_pwd}{$normal_color}
	end

	if set -q TMUX
		tmux set -q status-left $prompt
	else
		echo $prompt
	end
end
