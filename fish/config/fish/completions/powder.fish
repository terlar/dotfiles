# fish completion for powder
function __fish_powder
  powder help | grep powder | cut -d " " -f 4
end

complete -f -c powder -a '(__fish_powder)' -d 'Task'
