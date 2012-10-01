# fish completion for rake
function __rake_task_list_outdated --description 'Check if rake task cache is outdated'
  not test -f .rake_tasks; and return 0

  set -l files (find Rakefile lib/tasks -newer .rake_tasks 2>/dev/null)
  not test -z "$files"; and return 0

  return 1
end

function __fish_rake
  not test -f Rakefile; and return

  if __rake_task_list_outdated
    rake --silent --tasks | cut -d ' ' -f 2 | cut -d "[" -f 1 > .rake_tasks
  end

  cat .rake_tasks
end

complete -f -c rake -a '(__fish_rake)' -d 'Task'
