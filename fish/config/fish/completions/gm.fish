function __fish_git_branches
  git branch --no-color ^/dev/null | grep -v ' -> ' | sed -e 's/^..//' -e 's/^remotes\///'
end

complete -f -c gm -a '(__fish_git_branches)' -d 'Branch'

complete -f -c gm -l abort -d 'Abort the current conflict resolution process, and try to reconstruct the pre-merge state'

complete -f -c gr -l log -d 'In addition to branch names, populate the log message with one-line descriptions'
complete -f -c gr -l no-log -d 'Do not list one-line descriptions from the actual commits being merged'
complete -f -c gr -l no-ff -d 'Create a merge commit even when the merge resolves as a fast-forward'

complete -f -c gr -l squash -d 'Create a single commit on top of the current branch whose effect is the same as merging another branch'
complete -f -c gr -l no-squash -d 'Perform the merge and commit the result'
