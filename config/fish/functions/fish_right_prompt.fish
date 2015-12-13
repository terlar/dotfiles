function fish_right_prompt --description 'Write out the prompt'
	set last_status $status

	if test $last_status -ne 0
		set_color $fish_color_error
		echo -n [$last_status]
		set_color normal
	end

	set -q CMD_DURATION; or return
	test $CMD_DURATION -ge 100; or return
	set_color grey

	if test $CMD_DURATION -gt 60000
		set total_seconds (math $CMD_DURATION / 1000)
		if test $total_seconds -gt 3600
			set hours (math $total_seconds / 3600)
			set remainder (math $total_seconds '%' 3600)
			set minutes (math $remainder / 60)
			set seconds (math $remainder '%' 60)
			printf '(%dh%dm%ds)' $hours $minutes $seconds
		else if test $total_seconds -gt 60
			set minutes (math $total_seconds / 60)
			set seconds (math $total_seconds '%' 60)
			printf '(%dm%ds)' $minutes $seconds
		end
	else
		printf "(%'.2fs) " (string replace -r '(\d?)(\d{3})$' '$1.$2' $CMD_DURATION)
	end
	set_color normal
end
