function zeus_rake --description 'Run rake (with zeus if running)'
  if test -S .zeus.sock
    zeus rake $argv
  else
    command rake $argv
  end
end
