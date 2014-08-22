# fish completion for pacmd
function __fish_pacmd
	pacmd help |\
		grep '^  ' |\
		sed 's|^ *||; s|[[:blank:]]|'\t'|' |\
		unexpand -t1
end

complete -f -c pacmd -a '(__fish_pacmd)'
