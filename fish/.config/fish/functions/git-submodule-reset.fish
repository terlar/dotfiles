function git-submodule-reset --description 'Reset all changes to submodules'
    git submodule foreach --recursive 'git reset --hard; git clean -fd'
    git submodule update --init --recursive
end
