# fish completion for pacmd
function __fish_pacmd
	pacmd help | grep '^  ' | string replace -r '^\s+([^\s]+) +(.+)$' '$1'\t'$2'
end

complete -f -c pacmd -a '(__fish_pacmd)'
