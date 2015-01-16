function fish_right_prompt --description 'Write out the prompt'
	set -q CMD_DURATION; or return
	test $CMD_DURATION -ge 100; or return

	set_color white
	printf " (%'.2fs) " (echo $CMD_DURATION | sed 's/.\{3\}$/.&/')

	set_color normal
end
