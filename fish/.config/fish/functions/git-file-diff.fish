function git-file-diff --description 'List new and modified files'
    git show --pretty="format:" --name-only $argv | sort | uniq | grep -v '^$'
end
