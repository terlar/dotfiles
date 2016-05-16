function tmux_sessions
    tmux list-sessions | string replace -r ':.*' ''
end

complete -f -c mux -a '(tmux_sessions)'
