function __prompt_context_update --on-event fish_prompt
    set -l prompt_context_new (__prompt_context_build)
    if not test "$prompt_context_new" = "$__prompt_context_current"
        set -g __prompt_context_current $prompt_context_new
    end
end

function __prompt_context_reload -v __prompt_context_current
    echo $__prompt_context_current
end

function __prompt_context_build
    set prompt ' '
    set git_dir (command git rev-parse --show-toplevel ^/dev/null)

    if test -n "$git_dir"
        set project (string split '/' $git_dir)[-1]
        set short_pwd (string replace $git_dir $project $PWD)

        set branch (command git symbolic-ref --short --quiet HEAD)
        set dirty (command git diff --no-ext-diff --ignore-submodules --quiet; echo $status)
        set unmerged (command git cherry -v @\{upstream\} ^/dev/null)

        if test $dirty -ne 0
            set branch {$branch}
        else
            set branch {$branch}
        end

        if test -n "$unmerged"
            set branch $branch'⟳'
        end

        set prompt {$prompt}{$short_pwd}' on '{$branch}
    else
        set short_pwd (string replace -r "^$HOME" '~' $PWD)
        set prompt {$prompt}{$short_pwd}
    end

    if set -q TMUX
        tmux set -q status-left $prompt
    else
        echo $prompt
    end
end
