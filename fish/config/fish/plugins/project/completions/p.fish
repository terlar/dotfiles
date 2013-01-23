# fish completion for p
function __fish_projects
  set -l pattern (commandline -t)
  set -l list (ls $PROJECTS | grep $pattern)

  if test (count $list) -eq 1
    commandline -t $list
  else
    for i in $list
      echo $i
    end
  end
end

complete -f -c p -a '(__fish_projects)'
