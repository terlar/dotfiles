function fish_user_key_bindings
    type -q fzf_key_bindings
    and fzf_key_bindings

    emit fish_user_key_bindings

    #
    # Normal mode
    #
    bind e forward-word backward-char
    bind E forward-bigword backward-char

    #
    # Insert mode
    #
    bind -M insert \e'|' 'echo; hr; commandline -f repaint'

    # Navigation
    bind -M insert \e'<' 'prevd; echo; commandline -f repaint'
    bind -M insert \e'>' 'nextd; echo; commandline -f repaint'

    # Insert last argument of previous command
    bind -M insert \e. history-token-search-backward
end
