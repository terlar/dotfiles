function git
	if type -fq hub
		hub $argv
	else
		command git $argv
	end
end
