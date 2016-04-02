function git-watch-status
	watch -t --color "git -c color.status=always status -sb $argv"
end
