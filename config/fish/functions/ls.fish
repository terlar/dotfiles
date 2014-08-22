function ls --description 'List files with style'
	set argv -h $argv

	set -l with_color (command ls --color -d . ^/dev/null)

	if test -n "$with_color"
		set argv --color=tty $argv
	else
		set argv -G $argv
	end

	command ls $argv
end
