function git-branch-merged --description 'List branches that are merged'
	git branch -r --merged $argv | sed 's/^..origin\///' | grep -v $argv'$'
end
