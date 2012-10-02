function rc --description 'Run rails console (with zeus if running)'
  if test -S .zeus.sock
    zeus console
  else
    rails console
  end
end
