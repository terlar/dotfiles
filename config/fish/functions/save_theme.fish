function save_theme --description 'Save colorscheme'
	begin
		echo "function fish_theme --description 'Current theme'"
		for color in (set -n | grep color)
			echo set $color $$color
		end
		echo end
	end | fish_indent | source
	funcsave fish_theme
end
