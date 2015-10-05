function fish_right_prompt --description 'Write out the prompt'
	set -q CMD_DURATION; or return
	test $CMD_DURATION -ge 100; or return

	set_color white
	printf " (%'.2fs) " (string replace -r '(\d?)(\d{3})$' '$1.$2' $CMD_DURATION)
	set_color normal
end
