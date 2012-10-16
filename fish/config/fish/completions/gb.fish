function __fish_git_branches
  git branch --no-color -a 2>/dev/null | sed 's/^..//; s/^remotes\///'
end

complete -f -c gb -a '(__fish_git_branches)' -d 'Branch'
complete -f -c gb -s d -d 'Delete Branch'
complete -f -c gb -s D -d 'Force deletion of branch'
complete -f -c gb -s m -d 'Rename branch'
complete -f -c gb -s M -d 'Force renaming branch'
complete -f -c gb -s a -d 'Lists both local and remote branches'
