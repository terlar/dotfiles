function __fish_git_branches
  git branch --no-color ^/dev/null | grep -v ' -> ' | sed -e 's/^..//' -e 's/^remotes\///'
end

complete -f -c gr -a '(__fish_git_branches)' -d 'Branch'
complete -f -c gr -s i -l interactive -d 'Interactive mode'

complete -f -c gr -l continue -d 'Restart the rebasing process after having resolved a merge conflict'
complete -f -c gr -l abort -d 'Abort the rebase operation and reset HEAD to the original branch'
complete -f -c gr -l skip -d 'Restart the rebasing process by skipping the current patch'

complete -f -c gr -l no-ff -d 'No fast-forward'
