function __fish_ry_needs_command
  set cmd (commandline -opc)
  if [ (count $cmd) -eq 1 -a $cmd[1] = 'ry' ]
    return 0
  end
  return 1
end

function __fish_ry_using_command
  set cmd (commandline -opc)
  if [ (count $cmd) -gt 1 ]
    if [ $argv[1] = $cmd[2] ]
      return 0
    end
  end
  return 1
end

function __fish_ry_rubies
  ry ls
end

function __fish_ry_ruby_build
  ruby-build --definitions
end

complete -f -c ry -n '__fish_ry_needs_command' -a 'ls' -d 'Output the installed rubies'
complete -f -c ry -n '__fish_ry_needs_command' -a 'rubies' -d 'Output the installed rubies, and highlight the current one'

complete -f -c ry -a '(__fish_ry_rubies)' -d 'Ruby'
complete -f -c ry -n '__fish_ry_needs_command' -a 'use' -d 'Use the given ruby'
complete -f -c ry -n '__fish_ry_needs_command' -a 'rm remove' -d 'Remove the given ruby'

complete -f -c ry -n '__fish_ry_needs_command' -a 'install' -d 'Download, compile and install'
complete -f -c ry -n '__fish_ry_using_command install' -a '(__fish_ry_ruby_build)' -d 'Ruby'

complete -f -c ry -n '__fish_ry_needs_command' -a 'exec' -d 'Execute in the context of ruby'
complete -f -c ry -n '__fish_ry_needs_command' -a 'binpath' -d 'Print the bin directory for the given ruby'
complete -f -c ry -n '__fish_ry_needs_command' -a 'fullpath' -d 'Print $PATH including the given ruby\'s path.'

complete -f -c ry -n '__fish_ry_needs_command' -l help -s h -d 'Display help information'
complete -f -c ry -n '__fish_ry_needs_command' -l version -s V -d 'Output current version of ry'
