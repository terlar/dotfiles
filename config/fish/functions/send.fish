function send
	if test -d $argv
		command tar cj $argv
	else
		command cat $argv
	end | nc -lcp 1337
end
