function __fish_git_branches
  git branch --no-color -a ^/dev/null | grep -v ' -> ' | sed -e 's/^..//' -e 's/^remotes\///'
end

function __fish_git_tags
  git tag
end

complete -f -c gco -a '(__fish_git_branches)' --description 'Branch'
complete -f -c gco -a '(__fish_git_tags)' --description 'Tag'
complete -f -c gco -s b -d 'Create a new branch'
complete -f -c gco -s t -l track -d 'Track branch'
