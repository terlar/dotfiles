function read_confirm
	set -l prompt 'Do you want to continue?'
	set -l default 1

	for pair in (options $argv)
		echo $pair | read -l option value

		switch $option
			case p prompt
				set prompt $value
			case d default
				set default $value
		end
	end

	while true
		read -l -n 1 -p "echo '$prompt [Y/n] '" confirm

		switch $confirm
			case Y y
				return 0
			case N n
				return 1
			case ''
				return $default
		end
	end
end
