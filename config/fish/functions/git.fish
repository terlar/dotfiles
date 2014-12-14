function git
	if test (type hub ^/dev/null)
		hub $argv
	else
		command git $argv
	end
end
