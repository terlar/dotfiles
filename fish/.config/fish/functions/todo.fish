function todo --argument cmd
    set -q todo_file
    or set -U todo_file ~/TODO

    test -f "$todo_file"
    or touch "$todo_file"

    set -e argv[1]
    test -z "$cmd"
    and set cmd list

    switch "$cmd"
        case edit
            eval "$EDITOR" "$todo_file"
        case clear
            echo 'todo: you are about the clear the todo list'
            if read_confirm
                echo -n '' >"$todo_file"
            end
        case list ls show all grep find
            if test -z "$argv"
                cat "$todo_file"
            else
                grep "$argv" "$todo_file"
            end
        case next current pop up now
            head -n1 -- "$todo_file"
        case count size amount
            wc -l "$todo_file" | cut -b 1
        case add new create push append +
            echo $argv >>"$todo_file"
        case shift prepend ++
            set -l lines (cat "$todo_file")
            string join \n -- "$argv" $lines >"$todo_file"
        case done finish complete remove rm -
            if test -z "$argv"
                set argv 1
            end

            set -l task
            if string match -qr '^\d$' -- "$argv"
                set -l lines (cat "$todo_file")
                if not set -q lines[$argv]
                    echo "todo: unknown todo line '$argv'"
                    return 1
                end

                set task $lines[$argv]
                set -e lines[$argv]
                string split '\n' -- $lines >"$todo_file"
            else
                set -l matches (grep -- "$argv" "$todo_file")
                if test (count $matches) -gt 1
                    echo "todo: ambiguous match for '$argv'"
                    echo
                    echo 'Matched lines:'
                    string split '\n' -- $matches
                    return 1
                end

                set -l lines (grep -v -- "$argv" "$todo_file")
                string split '\n' -- $lines >"$todo_file"
                set task $matches
            end

            set_color green
            echo "✓ $task"
    end
end
