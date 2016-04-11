function __khal_using_command
	set cmd (commandline -opc)
	if test (count $cmd) -gt 1
		if [ $argv[1] = $cmd[2] ]
			return 0
		end
	end
	return 1
end

function __khal_commands
	set cmd (commandline -opc)
	if test (count $cmd) -gt 1
		return
	end

	khal --help | awk '/Commands:/{f=1;next}/^\s*$/{f=0}f' | string replace  -r '^\s+([^\s]+)\s+(.+)$' '$1'\t'$2'
end

complete -f -c khal -l config -s 'c' -d 'The config file to use.'
complete -f -c khal -l verbose -s 'v' -d 'Output debugging information.'
complete -f -c khal -l version -d 'Show the version and exit.'
complete -f -c khal -l help -d 'Show the help message and exit.'

complete -f -c khal -a '(__khal_commands)'

complete -f -c khal -n '__khal_using_command new' -s 'a' -d 'Specify a calendar to use.'
complete -f -c khal -n '__khal_using_command new' -s 'd' -d 'Specify a calendar to disregard.'
complete -f -c khal -n '__khal_using_command new' -l 'location' -s 'l' -d 'Location of event.'
complete -f -c khal -n '__khal_using_command new' -l 'until' -s 'u' -d 'Recurring until.'

complete -f -c khal -n '__khal_using_command new' -l 'repeat' -s 'r' -d 'Recurring event.'
complete -f -c khal -n '__fish_contains_opt -s r repeat' -d 'Repeat rule' -a 'daily weekly monthly yearly'

complete -f -c khal -n '__fish_contains_opt -s a' -d 'Calendar' -a '(khal printcalendars)'
complete -f -c khal -n '__fish_contains_opt -s d' -d 'Calendar' -a '(khal printcalendars)'
