function zeus_rspec --description 'Run rspec (with zeus if running)'
  if test -S .zeus.sock
    zeus test $argv
  else
    __pre_zeus_rspec $argv
  end
end
