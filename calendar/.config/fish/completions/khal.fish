### BEGIN GENERATED
function __khal_no_command
    set -l cmd (commandline -poc)
    set -e cmd[1]
    if test (count $cmd) -eq 0
        return 0
    end
    return 1
end

function __khal_using_command
    set cmd (commandline -poc)
    if test (count $cmd) -gt 1
        if test $argv[1] = $cmd[2]
            return 0
        end
    end
    return 1
end

complete -c khal -n "__khal_using_command printformats" -l "help" -d "Show this message and exit."
complete -c khal -n "__khal_using_command printcalendars" -s "d" -d "Exclude the given calendar. Can be specified"
complete -c khal -n "__khal_using_command printcalendars" -l "exclude-calendar" -d "Exclude the given calendar. Can be specified"
complete -c khal -n "__khal_using_command printcalendars" -s "a" -d "Include the given calendar. Can be specified"
complete -c khal -n "__khal_using_command printcalendars" -l "include-calendar" -d "Include the given calendar. Can be specified"
complete -c khal -n "__khal_using_command printcalendars" -l "help" -d "Show this message and exit."
complete -c khal -n "__khal_using_command at" -s "d" -d "Exclude the given calendar. Can be specified"
complete -c khal -n "__khal_using_command at" -l "exclude-calendar" -d "Exclude the given calendar. Can be specified"
complete -c khal -n "__khal_using_command at" -s "a" -d "Include the given calendar. Can be specified"
complete -c khal -n "__khal_using_command at" -l "include-calendar" -d "Include the given calendar. Can be specified"
complete -c khal -n "__khal_using_command at" -l "help" -d "Show this message and exit."
complete -c khal -n "__khal_using_command new" -s "a" -d "CAL"
complete -c khal -n "__khal_using_command new" -l "calendar" -d "CAL"
complete -c khal -n "__khal_using_command new" -s "l" -d "The location of the new event."
complete -c khal -n "__khal_using_command new" -l "location" -d "The location of the new event."
complete -c khal -n "__khal_using_command new" -s "r" -d "Repeat event: daily, weekly, monthly or yearly."
complete -c khal -n "__khal_using_command new" -l "repeat" -d "Repeat event: daily, weekly, monthly or yearly."
complete -c khal -n "__khal_using_command new" -s "u" -d "Stop an event repeating on this date."
complete -c khal -n "__khal_using_command new" -l "until" -d "Stop an event repeating on this date."
complete -c khal -n "__khal_using_command new" -l "help" -d "Show this message and exit."
complete -c khal -n "__khal_using_command import" -s "a" -d "The calendar to use."
complete -c khal -n "__khal_using_command import" -l "include-calendar" -d "The calendar to use."
complete -c khal -n "__khal_using_command import" -l "batch" -d "do not ask for any confirmation."
complete -c khal -n "__khal_using_command import" -s "r" -d "Select a random uid."
complete -c khal -n "__khal_using_command import" -l "random_uid" -d "Select a random uid."
complete -c khal -n "__khal_using_command import" -l "help" -d "Show this message and exit."
complete -c khal -f -n "__khal_no_command" -a agenda -d "Print agenda."
complete -c khal -f -n "__khal_no_command" -a at -d "Show all events scheduled for DATETIME."
complete -c khal -f -n "__khal_no_command" -a calendar -d "Print calendar with agenda."
complete -c khal -f -n "__khal_no_command" -a import -d "Import events from an .ics file."
complete -c khal -f -n "__khal_no_command" -a interactive -d "Interactive UI."
complete -c khal -f -n "__khal_no_command" -a new -d "Create a new event from this command's..."
complete -c khal -f -n "__khal_no_command" -a printcalendars -d "List all calendars."
complete -c khal -f -n "__khal_no_command" -a printformats -d "Print a date in all formats."
complete -c khal -f -n "__khal_no_command" -a search -d "Search for events matching SEARCH_STRING."
complete -c khal -s "c" -d "The config file to use."
complete -c khal -l "config" -d "The config file to use."
complete -c khal -s "v" -d "Output debugging information."
complete -c khal -l "verbose" -d "Output debugging information."
complete -c khal -l "version" -d "Show the version and exit."
complete -c khal -l "help" -d "Show this message and exit. agenda          Print agenda."
complete -c khal -n "__khal_using_command calendar" -l "events" -d "How many events to include."
complete -c khal -n "__khal_using_command calendar" -l "days" -d "How many days to include."
complete -c khal -n "__khal_using_command calendar" -s "d" -d "Exclude the given calendar. Can be specified"
complete -c khal -n "__khal_using_command calendar" -l "exclude-calendar" -d "Exclude the given calendar. Can be specified"
complete -c khal -n "__khal_using_command calendar" -s "a" -d "Include the given calendar. Can be specified"
complete -c khal -n "__khal_using_command calendar" -l "include-calendar" -d "Include the given calendar. Can be specified"
complete -c khal -n "__khal_using_command calendar" -s "f" -d "Print description and location with event"
complete -c khal -n "__khal_using_command calendar" -l "full" -d "Print description and location with event"
complete -c khal -n "__khal_using_command calendar" -l "help" -d "Show this message and exit."
complete -c khal -n "__khal_using_command interactive" -s "d" -d "Exclude the given calendar. Can be specified"
complete -c khal -n "__khal_using_command interactive" -l "exclude-calendar" -d "Exclude the given calendar. Can be specified"
complete -c khal -n "__khal_using_command interactive" -s "a" -d "Include the given calendar. Can be specified"
complete -c khal -n "__khal_using_command interactive" -l "include-calendar" -d "Include the given calendar. Can be specified"
complete -c khal -n "__khal_using_command interactive" -l "help" -d "Show this message and exit."
complete -c khal -n "__khal_using_command search" -s "d" -d "Exclude the given calendar. Can be specified"
complete -c khal -n "__khal_using_command search" -l "exclude-calendar" -d "Exclude the given calendar. Can be specified"
complete -c khal -n "__khal_using_command search" -s "a" -d "Include the given calendar. Can be specified"
complete -c khal -n "__khal_using_command search" -l "include-calendar" -d "Include the given calendar. Can be specified"
complete -c khal -n "__khal_using_command search" -l "help" -d "Show this message and exit."
complete -c khal -n "__khal_using_command agenda" -l "events" -d "How many events to include."
complete -c khal -n "__khal_using_command agenda" -l "days" -d "How many days to include."
complete -c khal -n "__khal_using_command agenda" -s "d" -d "Exclude the given calendar. Can be specified"
complete -c khal -n "__khal_using_command agenda" -l "exclude-calendar" -d "Exclude the given calendar. Can be specified"
complete -c khal -n "__khal_using_command agenda" -s "a" -d "Include the given calendar. Can be specified"
complete -c khal -n "__khal_using_command agenda" -l "include-calendar" -d "Include the given calendar. Can be specified"
complete -c khal -n "__khal_using_command agenda" -s "f" -d "Print description and location with event"
complete -c khal -n "__khal_using_command agenda" -l "full" -d "Print description and location with event"
complete -c khal -n "__khal_using_command agenda" -l "help" -d "Show this message and exit."
### END GENERATED

complete -c khal -f -n '__fish_contains_opt -s r repeat' -d 'Repeat rule' -a 'daily weekly monthly yearly'
complete -c khal -f -n '__fish_contains_opt -s a include-calendar' -d 'Calendar' -a '(khal printcalendars)'
complete -c khal -f -n '__fish_contains_opt -s d exclude-calendar' -d 'Calendar' -a '(khal printcalendars)'
