function fish_user_key_bindings
  bind \e! __runsudo
end

function __runsudo --description 'Run current command line as root'
  commandline -C 0
  commandline -i 'sudo '
  commandline -f execute
end
