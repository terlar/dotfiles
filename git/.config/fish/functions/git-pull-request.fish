function git-pull-request
    set -l upstream_ref (command git rev-parse --abbrev-ref '@{u}')

    if not string match -q 'origin/*' $upstream_ref
        command git push --set-upstream origin (command git rev-parse --abbrev-ref @)
    end

    if test (count $argv) -eq 1
        hub pull-request -b $argv
    else
        hub pull-request $argv
    end
end
