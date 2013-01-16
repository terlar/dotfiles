function __fish_git_branches
  git branch --no-color ^/dev/null | grep -v ' -> ' | sed -e 's/^..//' -e 's/^remotes\///'
end

complete -f -c gm -a '(__fish_git_branches)' -d 'Branch'
