function fish_title
    set -l command (echo $_)

    if test $command = "fish"
        echo $__prompt_context_current
    else
        echo $command
    end
end
