# Commandline watch toggle
function __watch_toggle_kb -e fish_user_key_bindings
    bind \ew __commandline_watch_toggle
end

function __commandline_watch_toggle
    set pos (commandline -C)
    set cmd (commandline)

    if string match -q 'watch -n1 *' $cmd
        set pos (expr $pos - 9)
        commandline (string replace 'watch -n1 ' '' $cmd)
    else
        commandline -C 0
        commandline -i 'watch -n1 '
        set pos (expr $pos + 9)
    end

    commandline -C $pos
end
