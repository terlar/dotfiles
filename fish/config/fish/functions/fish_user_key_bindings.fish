function fish_user_key_bindings
  # Accept or go to end of line
  bind \ce accept-autosuggestion

  # Insert last argument of previous command
  bind \e. history-token-search-backward

  bind \e! __runsudo
end

function __runsudo --description 'Run current command line as root'
  commandline -C 0
  commandline -i 'sudo '
  commandline -f execute
end
