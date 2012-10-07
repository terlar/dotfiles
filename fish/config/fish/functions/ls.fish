function ls --description 'List files with style'
  set argv -h $argv

  if command ls --color -d . >/dev/null 2>&1
    set argv --color=tty $argv
  else
    set argv -G $argv
  end

  command ls $argv
end