function __fish_todo_needs_command
    set cmd (commandline -opc)
    if [ (count $cmd) -eq 1 ]
        return 0
    end
    return 1
end

function __fish_todo_using_command
    set cmd (commandline -opc)
    if [ (count $cmd) -gt 1 ]
        if [ $argv[1] = $cmd[2] ]
            return 0
        end
    end
    return 1
end

complete -f -c todo -n '__fish_todo_needs_command' -a 'edit' -d "Edit notes in $EDITOR"
complete -f -c todo -n '__fish_todo_needs_command' -a 'clear' -d 'Clear all todos'

for i in list ls show all grep find
    complete -f -c todo -n '__fish_todo_needs_command' -a $i -d 'List todos'
end

for i in next current pop up now
    complete -f -c todo -n '__fish_todo_needs_command' -a $i -d 'Show next todo'
end

for i in count size amount
    complete -f -c todo -n '__fish_todo_needs_command' -a $i -d 'Show todo count'
end

for i in add new create push append +
    complete -f -c todo -n '__fish_todo_needs_command' -a $i -d 'Add a todo'
end

for i in shift prepend ++
    complete -f -c todo -n '__fish_todo_needs_command' -a $i -d 'Add a todo'
end

for i in done finish complete remove rm -
    complete -f -c todo -n '__fish_todo_needs_command' -a $i -d 'Complete a todo'
    complete -f -c todo -n "__fish_todo_using_command $i" -a '(todo list)' -d 'Note'
end
