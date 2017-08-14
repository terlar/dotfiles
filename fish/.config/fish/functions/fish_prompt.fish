function fish_prompt --description 'Write out the prompt'
    if test $status -ne 0
        set_color $fish_color_error
    else
        set_color normal
        set_color grey
    end

    echo -n '$ '
    set_color normal
end
