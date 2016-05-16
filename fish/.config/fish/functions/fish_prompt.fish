function fish_prompt --description 'Write out the prompt'
    set -l last_status $status

    set -l prompt_context_new (prompt_context)
    if not test "$prompt_context_new" = "$__prompt_context_current"
        set -g __prompt_context_current $prompt_context_new
    end

    if test $last_status -ne 0
        set_color $fish_color_error
    else
        set_color normal
        set_color grey
    end

    echo -n '$ '
    set_color normal
end
