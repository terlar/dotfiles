# fish completion for powder
function __fish_powder
	powder help | grep powder | sed 's/^	powder //; s/[[:blank:]].*#/'\t'/g'
end

complete -f -c powder -a '(__fish_powder)'
