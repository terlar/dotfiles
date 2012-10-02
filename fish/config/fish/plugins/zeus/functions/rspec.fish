function rspec --description 'Run rspec (with zeus if running)'
  if test -S .zeus.sock
    zeus test $argv
  else
    __bundle_run rspec $argv
  end
end
