### BEGIN GENERATED
function __ghq_no_command
    set -l cmd (commandline -poc)
    set -e cmd[1]
    if test (count $cmd) -eq 0
        return 0
    end
    return 1
end

function __ghq_using_command
    set cmd (commandline -poc)
    if test (count $cmd) -gt 1
        if test $argv[1] = $cmd[2]
            return 0
        end
    end
    return 1
end

function __ghq_max_arg_count
    set cmd (commandline -poc)
    set -e cmd[1]
    if test (count $cmd) -lt $argv[1]
        return 0
    end
    return 1

end

complete -c ghq -n "__ghq_using_command import" -s "u" -d "Update local repository if cloned already"
complete -c ghq -n "__ghq_using_command import" -l "update" -d "Update local repository if cloned already"
complete -c ghq -n "__ghq_using_command import" -s "p" -d "Clone with SSH"
complete -c ghq -n "__ghq_using_command import" -l "shallow" -d "Do a shallow clone"
complete -c ghq -n "__ghq_using_command list" -s "e" -d "Perform an exact match"
complete -c ghq -n "__ghq_using_command list" -l "exact" -d "Perform an exact match"
complete -c ghq -n "__ghq_using_command list" -s "p" -d "Print full paths"
complete -c ghq -n "__ghq_using_command list" -l "full-path" -d "Print full paths"
complete -c ghq -n "__ghq_using_command list" -l "unique" -d "Print unique subpaths"
complete -c ghq -n "__ghq_using_command root" -l "all" -d "Show all roots"
complete -c ghq -n "__ghq_using_command get" -s "u" -d "Update local repository if cloned already"
complete -c ghq -n "__ghq_using_command get" -l "update" -d "Update local repository if cloned already"
complete -c ghq -n "__ghq_using_command get" -s "p" -d "Clone with SSH"
complete -c ghq -n "__ghq_using_command get" -l "shallow" -d "Do a shallow clone"
complete -c ghq -f -n "__ghq_no_command" -a get -d "Clone/sync with a remote repository"
complete -c ghq -f -n "__ghq_no_command" -a list -d "List local repositories"
complete -c ghq -f -n "__ghq_no_command" -a look -d "Look into a local repository"
complete -c ghq -f -n "__ghq_no_command" -a import -d "Bulk get repositories from stdin"
complete -c ghq -f -n "__ghq_no_command" -a root -d "Show repositories' root"
complete -c ghq -s "h" -d "show help"
complete -c ghq -l "help" -d "show help"
complete -c ghq -s "v" -d "print the version"
complete -c ghq -l "version" -d "print the version"
### END GENERATED

complete -c ghq -f
complete -c ghq -f -n "__ghq_using_command look; and __ghq_max_arg_count 2" -a '(ghq list)' -d 'Repo'
