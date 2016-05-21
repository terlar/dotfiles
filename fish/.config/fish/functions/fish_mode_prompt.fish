function fish_mode_prompt --description 'Displays the current mode'
    # Do nothing if not in vi mode
    if test "$fish_key_bindings" = "fish_vi_key_bindings"
        switch $fish_bind_mode
            case default
                set_color red
            case insert
                set_color green
            case replace-one
                set_color green
            case visual
                set_color magenta
        end
        echo -n '■'
        set_color normal
    end
end
