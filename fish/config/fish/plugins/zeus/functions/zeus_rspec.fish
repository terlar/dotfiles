function zeus_rspec --description 'Run rspec (with zeus if running)'
  if test -S .zeus.sock
    zeus test $argv
  else
    command rspec $argv
  end
end
