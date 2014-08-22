function fish_right_prompt --description 'Write out the prompt'
	set -q CMD_DURATION; or return
	test $CMD_DURATION -ge 100; or return

	set_color white
	printf '(%gs) ' (math "scale=3; $CMD_DURATION / 1000")
	set_color normal
end
