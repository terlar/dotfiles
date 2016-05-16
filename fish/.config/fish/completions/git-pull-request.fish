function __fish_git_unique_remote_branches
    # Allow all remote branches with one remote without the remote part
    # This is useful for `git checkout` to automatically create a remote-tracking branch
    command git branch --no-color -a ^/dev/null \
        | __fish_sgrep -v ' -> ' \
        | __fish_sgrep 'remotes/origin' \
        | string trim -c '* ' \
        | string replace -r '^remotes/origin/' '' \
        | sort \
        | uniq -u
end

complete -f -c git-pull-request -a '(__fish_git_unique_remote_branches)' -d 'Remote branch'
