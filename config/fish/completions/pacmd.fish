# fish completion for pacmd
function __fish_pacmd
	pacmd help | grep '^  ' | string trim | string replace -r ' +' \t
end

complete -f -c pacmd -a '(__fish_pacmd)'
