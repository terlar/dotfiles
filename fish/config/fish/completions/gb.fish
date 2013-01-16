function __fish_git_branches
  git branch --no-color -a ^/dev/null | grep -v ' -> ' | sed -e 's/^..//' -e 's/^remotes\///'
end

complete -f -c gb -a '(__fish_git_branches)' -d 'Branch'
complete -f -c gb -s d -d 'Delete branch'
complete -f -c gb -s D -d 'Force deletion of branch'
complete -f -c gb -s m -d 'Rename branch'
complete -f -c gb -s M -d 'Force renaming branch'
complete -f -c gb -s a -d 'Lists both local and remote branches'
complete -f -c gb -s t -l track -d 'Track remote branch'
complete -f -c gb -l no-track -d 'Do not track remote branch'
complete -f -c gb -l set-upstream -d 'Change remote branch tracking'
