function __fish_shovel_needs_command
	set cmd (commandline -opc)
	if [ (count $cmd) -eq 1 -a $cmd[1] = 'shovel' ]
		return 0
	end
	return 1
end

function __fish_shovel_using_command
	set cmd (commandline -opc)
	if [ (count $cmd) -gt 1 ]
		if [ $argv[1] = $cmd[2] ]
			return 0
		end
	end
	return 1
end

function __fish_shovel_commands
	shovel help | string replace ': ' \t
end

function __fish_shovel_overrides
	shovel override | grep '-' | string replace ' - ' ''
end

complete -f -c shovel -n '__fish_shovel_needs_command' -a '(__fish_shovel_commands)'
complete -f -c shovel -n '__fish_shovel_using_command override' -a '(__fish_shovel_overrides)' -d 'Service'
