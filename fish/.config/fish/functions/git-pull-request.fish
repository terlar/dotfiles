function git-pull-request
	set -l upstream_ref (git rev-parse --abbrev-ref '@{u}')

	if not string match -q 'origin/*' $upstream_ref
		git push --set-upstream origin (git rev-parse --abbrev-ref @)
	end

	if test (count $argv) -eq 1
		git pull-request -b $argv
	else
		git pull-request $argv
	end
end
