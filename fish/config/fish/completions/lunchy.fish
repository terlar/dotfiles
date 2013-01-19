# fish completion for lunchy
function __fish_lunchy_needs_command
  set cmd (commandline -opc)
  if [ (count $cmd) -eq 1 -a $cmd[1] = 'lunchy' ]
    return 0
  end
  return 1
end

function __fish_lunchy_using_command
  set arg_pos 1
  if test (count $argv) = 2
    set arg_pos $argv[2]
  end

  set cmd (commandline -opc)
  if [ (count $cmd) -gt $arg_pos ]
    if [ $argv[1] = $cmd[2] ]
      return 0
    end
  end
  return 1
end

function __fish_lunchy_services
  lunchy ls
end

complete -f -c lunchy -s v -l verbose -d 'Show command executions'

complete -f -c lunchy -n '__fish_lunchy_needs_command' -a 'ls list' -d 'Show the list of installed agents'
complete -f -c lunchy -n '__fish_lunchy_needs_command' -a start -d 'Start the first matching agent'
complete -f -c lunchy -n '__fish_lunchy_needs_command' -a stop -d 'Stop the first matching agent'
complete -f -c lunchy -n '__fish_lunchy_needs_command' -a restart -d 'Stop and start the first matching agent'
complete -f -c lunchy -n '__fish_lunchy_needs_command' -a status -d 'Show the PID and label for all agents'
complete -f -c lunchy -n '__fish_lunchy_needs_command' -a install -d 'Installs [file] to ~/Library/LaunchAgents or /Library/LaunchAgents'
complete -f -c lunchy -n '__fish_lunchy_needs_command' -a edit -d 'Opens the launchctl daemon file in the default editor'

complete -f -c lunchy -n '__fish_lunchy_using_command start' -a '(__fish_lunchy_services)' -d 'Service'
complete -f -c lunchy -n '__fish_lunchy_using_command start' -s w -l write -d 'Persist command'
complete -f -c lunchy -n '__fish_lunchy_using_command start' -s F -l force -d 'Force start (disabled) agents'

complete -f -c lunchy -n '__fish_lunchy_using_command stop' -a '(__fish_lunchy_services)' -d 'Service'
complete -f -c lunchy -n '__fish_lunchy_using_command stop' -s w -l write -d 'Persist command'

complete -f -c lunchy -n '__fish_lunchy_using_command restart' -a '(__fish_lunchy_services)' -d 'Service'
complete -f -c lunchy -n '__fish_lunchy_using_command status' -a '(__fish_lunchy_services)' -d 'Service'
complete -f -c lunchy -n '__fish_lunchy_using_command edit' -a '(__fish_lunchy_services)' -d 'Service'
