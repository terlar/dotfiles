function editor --description 'Wrapper for editor'
    if set -q DISPLAY
        eval $VISUAL $argv
    else
        eval $EDITOR $argv
    end
end
