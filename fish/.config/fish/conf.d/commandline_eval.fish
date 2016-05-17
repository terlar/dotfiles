# Commandline eval
function __commandline_eval_kb -e fish_user_key_bindings
    bind -M insert \ex __commandline_eval_token
end

function __commandline_eval_token
    set token (commandline -t)
    test -n "$token"
    or return

    set value (eval string escape $token | string join ' ')
    test -n "$value"
    or return

    commandline -t $value
    if string match -q '*/' $value
        test -d $value
        and return
        mkdir -p $value
        commandline -t $value
    end
end
