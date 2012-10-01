function l --description 'List files with style'
  ls --color -d . 2>/dev/null; and ls --color=tty -hF $argv; or ls -G -hF $argv
end
