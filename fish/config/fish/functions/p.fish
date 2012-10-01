function p --description 'Switch project'
  if count $argv >/dev/null
    cd $PROJECTS/$argv[1]*
  else
    cd $PROJECTS
  end
end
