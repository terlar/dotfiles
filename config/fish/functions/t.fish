function t --description 'Attach or open new tmux session'
	if test (count $argv) -gt 0
		tmux attach -t $argv[1]
		or tmux new -s $argv[1]
	else
		tmux attach
		or tmux new
	end
end
