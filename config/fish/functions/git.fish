function git
	if which hub >/dev/null ^/dev/null
		hub $argv
	else
		command git $argv
	end
end
