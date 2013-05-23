function ls --description 'List files with style'
  set argv -h $argv

  if test (command ls --color -d . ^/dev/null)
    set argv --color=tty $argv
  else
    set argv -G $argv
  end

  command ls $argv
end
