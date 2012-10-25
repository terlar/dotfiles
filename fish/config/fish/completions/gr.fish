function __fish_git_branches
  git branch --no-color -a 2>/dev/null | sed 's/^..//; s/^remotes\///'
end

complete -f -c gr -a '(__fish_git_branches)' -d 'Branch'
