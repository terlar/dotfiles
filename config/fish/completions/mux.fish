function tmux_sessions
	tmux list-sessions | cut -d ':' -f 1 ^/dev/null
end

complete -f -c mux -a '(tmux_sessions)'
