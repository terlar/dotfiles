function git-file-diff --description 'List new and modified files'
    command git show --pretty="format:" --name-only $argv | sort --unique | grep -v '^$'
end
