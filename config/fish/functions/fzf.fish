function fzf
	if test -z (git rev-parse HEAD ^/dev/null)
		set -eg FZF_DEFAULT_COMMAND
	else
		set -gx FZF_DEFAULT_COMMAND 'git ls-tree -r --name-only HEAD'
	end

	~/.fzf/bin/fzf $argv
end
