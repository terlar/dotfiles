function git-submodule-reset --description 'Reset all changes to submodules'
    command git submodule foreach --recursive 'git reset --hard; git clean -fd'
    command git submodule update --init --recursive
end
