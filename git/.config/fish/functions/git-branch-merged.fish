function git-branch-merged --description 'List branches that are merged into branch'
    command git for-each-ref --format='%(refname:short)' --merged $argv | grep -v $argv'$'
end
