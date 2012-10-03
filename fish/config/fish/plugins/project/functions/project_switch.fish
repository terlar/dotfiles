function project_switch --description 'Switch project'
  if test (count $argv) = 0
    cd $PROJECTS
  else
    cd $PROJECTS/$argv[1]*
  end
end
