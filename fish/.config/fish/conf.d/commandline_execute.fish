# Commandline execute
function __commandline_execute_kb -e fish_user_key_bindings
    # Skip execution for empty lines
    bind \r __commandline_execute_non_empty
    bind \n __commandline_execute_non_empty
    # Execute and restore the line after execution
    bind \ej __commandline_execute_and_keep_line
    # Line-complete and execute
    bind \ee end-of-line execute
end

function __commandline_execute_non_empty
    set -l buffer (commandline -b)
    if test -n "$buffer"
        commandline -f execute
    end
end

function __commandline_execute_and_keep_line
    set -l buffer (commandline -b)

    # If there is no commandline buffer, restore from history.
    if test -z $buffer
        commandline -f history-search-backward
        return
    end

    commandline -f execute

    while true
        set funcname __fish_restore_line_(random)
        if not functions $funcname >/dev/null ^/dev/null
            break
        end
    end

    function $funcname -V buffer -V funcname -j %self
        commandline -r -- $buffer
        functions -e $funcname
    end
end
