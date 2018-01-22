function git-repos-dirty -d 'List all repos that are dirty'
    for repo in (ghq list -p)
        set result (git -C $repo diff --shortstat)
        if test -n "$result"
            echo $repo
            echo $result
            echo
        end
    end
end
