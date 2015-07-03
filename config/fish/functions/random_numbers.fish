function random_numbers
	echo $argv | read -l i max

	if test -z $max
		set max 10
	end

	while true
		if test $i = 0
			break
		end

		math (random)" % $max"
		set i (math $i - 1)
	end
end
