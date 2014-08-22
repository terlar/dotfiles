function dirdiff --description 'Compare two directories'
	vimdiff \
		(ls -R $argv[1]|sed 's/.*\/\(.*\)/\1/'|psub)\
		(ls -R $argv[2]|sed 's/.*\/\(.*\)/\1/'|psub)
end
