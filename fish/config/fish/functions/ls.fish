function ls --description 'List files with style'
  set argv -h $argv

  if /bin/ls --color -d . 2>/dev/null
    set argv --color=tty $argv
  else
    set argv -G $argv
  end

  /bin/ls $argv
end
