function __fish_git_aliases
  git config --get-regexp alias | sed "s/^alias\.\([^ ]*\).*/\1/"
end

function __fish_git_needs_command
  set cmd (commandline -opc)
  if [ (count $cmd) -eq 1 -a $cmd[1] = 'g' ]
    return 0
  end
  return 1
end

function __fish_git_using_command
  set cmd (commandline -opc)
  if [ (count $cmd) -gt 1 ]
    if [ $argv[1] = $cmd[2] ]
      return 0
    end
  end
  return 1
end

complete -f -c g

# Aliases
complete -c g -n '__fish_git_needs_command' -a '(__fish_git_aliases)' -d 'Alias'

# stash
complete -c g -n '__fish_git_needs_command' -a stash -d 'Stash away changes'
complete -f -c g -n '__fish_git_using_command stash' -a list -d 'List stashes'
complete -f -c g -n '__fish_git_using_command stash' -a show -d 'Show stashed changes'
complete -f -c g -n '__fish_git_using_command stash' -a pop -d 'Apply and remove stashed changes'
complete -f -c g -n '__fish_git_using_command stash' -a apply -d 'Apply stashed changes'
complete -f -c g -n '__fish_git_using_command stash' -a drop -d 'Remove stash'
complete -f -c g -n '__fish_git_using_command stash' -a clear -d 'Remove all stashes'
