function copy --description 'Trim new lines and copy to clipboard'
	cat $argv ^/dev/null | while read -l line
		set argv $argv $line
	end

	test -z "$argv"; and return

	for i in $argv
		echo -n $i
	end | tr -d '\n' | xclip
end
