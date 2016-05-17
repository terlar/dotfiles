# Commandline execute
function __commandline_execute_kb -e fish_user_key_bindings
    # Normal mode
    bind \n '__commandline_execute'

    # Insert mode
    bind -M insert \n 'commandline -i \n'
    bind -M insert \r __commandline_execute
    bind -M insert \e\r __commandline_execute_and_keep_line
    bind -M insert \e\n 'commandline -f accept-autosuggestion execute'
    bind -M insert \e',' 'commandline -f history-search-backward execute'
    bind -M insert \ez 'fg >/dev/null ^/dev/null'
end

function __commandline_execute
    set -l cmd (commandline)
    if test -z "$cmd"
        echo
    else
        commandline -f execute
    end
end

function __commandline_execute_and_keep_line
    __commandline_stash
    commandline $command_stash[-1]
    commandline -f execute

    while true
        set funcname __fish_restore_line_(random)

        if not functions $funcname >/dev/null ^/dev/null
            break

        end
    end

    function $funcname -V funcname -j %self
        __commandline_pop
        functions -e $funcname
    end
end
