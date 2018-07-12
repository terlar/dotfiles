function fish_right_prompt --description 'Write out the prompt'
    set last_status $status

    if set -q cmd_duration
        set_color grey
        print_duration $cmd_duration
        set_color normal
    end

    if test $last_status -ne 0
        set_color $fish_color_error
        echo -n [$last_status]
        set_color normal
    end
end
