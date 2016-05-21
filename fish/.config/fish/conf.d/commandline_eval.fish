# Commandline eval
function __commandline_eval_kb -e fish_user_key_bindings
    bind -M insert \ex __commandline_eval_token
end

function __commandline_eval_token
    set -l tokens (eval string escape -- (commandline -ct))
    set -q tokens[1]
    or return

    commandline -tr ''
    commandline -i -- "$tokens"

    # Create directory if not existing. Needs to end with trailing `/`.
    if string match -q -- '*/' "$tokens"
        test -d "$tokens"
        and return
        mkdir -p "$tokens"
        commandline -t -- "$tokens"
    end
end
