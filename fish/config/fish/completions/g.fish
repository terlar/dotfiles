function __fish_git_aliases
  git config --get-regexp alias | sed "s/^alias\.\([^ ]*\).*/\1/"
end

complete -f -c g -a '(__fish_git_aliases)' -d 'Alias'
