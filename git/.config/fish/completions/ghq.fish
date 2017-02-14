# ghq - provides a way to organize remote repository clones, like go get
# does. When you clone a remote repository by ghq get, ghq makes a
# directory under a specific root directory using the remote repository
# URL’s host and path.
# See: https://github.com/motemen/ghq

function __ghq_using_command
    set -l cmd (commandline -poc)
    set -l found

    test (count $cmd) -gt (count $argv)
    or return 1

    set -e cmd[1]

    for i in $argv
        contains -- $i $cmd
        and set found $found $i
    end

    test "$argv" = "$found"
end

function __ghq_seen_any_subcommand_from -a cmd
    __fish_seen_subcommand_from (__ghq_subcommands $cmd | string match -r '\S+')
end

function __ghq_subcommands -a cmd
    switch $cmd
        case ''
            echo get\t'Clone/sync with a remote repository'
            echo list\t'List local repositories'
            echo look\t'Look into a local repository'
            echo import\t'Bulk get repositories from stdin'
            echo root\t"Show repositories' root"
    end
end

function __ghq_seen_project
    string match -q -- '* */* *' (commandline -p)
end

function __ghq_github_repo_completion -a term
    test (string length $term) -gt 3
    or return

    type -q curl
    or return

    type -q jq
    or return

    if string match -q -- '*/*' $term
        set -l usr (string match -r -- '^[^\s/]+' $term)
        curl -sS "https://api.github.com/search/repositories?q=user:$usr" | jq -r '.items[] | .full_name + "\t" + .description' ^/dev/null
    else
        curl -sS "https://api.github.com/search/users?q=$term+in:login" | jq -r '.items[].login + "/"' ^/dev/null
    end
end

#
# Global options
#
complete -c ghq -s h -l help -f -d 'Show help'
complete -c ghq -s v -l version -f -d 'Print the version'

#
# Commands
#

# ghq [global options] command [command options] [arguments...]
complete -c ghq -n 'not __ghq_seen_any_subcommand_from ""' -x -a '(__ghq_subcommands "")'

# ghq get [-u] <repository URL> | [-u] [-p] <user>/<project>
complete -c ghq -n '__ghq_using_command get; and not __ghq_seen_project' -a '(__ghq_github_repo_completion (commandline -pt))' -f -d 'Repo'

complete -c ghq -n '__ghq_using_command get' -s u -l update -f -d 'Update local repository if cloned already'
complete -c ghq -n '__ghq_using_command get' -s p -f -d 'Clone with SSH'
complete -c ghq -n '__ghq_using_command get' -l shallow -f -d 'Do a shallow clone'

# ghq list [-p] [-e] [<query>]
complete -c ghq -n '__ghq_using_command list' -s e -l exact -f -d 'Perform an exact match'
complete -c ghq -n '__ghq_using_command list' -s p -l full-path -f -d 'Print full paths'
complete -c ghq -n '__ghq_using_command list' -l unique -f -d 'Print unique subpaths'

# ghq look <project> | <user>/<project> | <host>/<user>/<project>
complete -c ghq -n '__ghq_using_command look; and not __ghq_seen_project' -x -a '(ghq list)' -d 'Project'

# ghq import < file
complete -c ghq -n '__ghq_using_command import' -s u -l update -f -d 'Update local repository if cloned already'
complete -c ghq -n '__ghq_using_command import' -s p -f -d 'Clone with SSH'
complete -c ghq -n '__ghq_using_command import' -l shallow -f -d 'Do a shallow clone'

# ghq root
complete -c ghq -n '__ghq_using_command root' -l all -f -d 'Show all roots'
