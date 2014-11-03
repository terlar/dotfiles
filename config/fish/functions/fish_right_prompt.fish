function fish_right_prompt --description 'Write out the prompt'
	if test $fish_key_bindings = 'fish_vi_key_bindings'
		switch $fish_bind_mode
		case default
			set_color --bold --background red white
			echo '[N]'
		case insert
			set_color --bold --background green white
			echo '[I]'
		case visual
			set_color --bold --background magenta white
			echo '[V]'
		end
	end

	set_color normal

	set -q CMD_DURATION; or return
	test $CMD_DURATION -ge 100; or return

	set_color white
	printf ' (%gs) ' (math "scale=3; $CMD_DURATION / 1000")

	set_color normal
end
