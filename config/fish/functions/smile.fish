function smile --description 'Smiley dictionary'
	cat ~/.smile | grep "$argv" | cut -d ' ' -f 2-
end
