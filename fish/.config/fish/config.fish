set -Uq fish_setup_done
or fish_setup

if test "$TERM" = eterm-color
    function fish_title
    end
end

plug terlar/fry
plug terlar/fish-farm

# Environment
if status --is-login
    and test -f ~/.env

    for line in (~/.env)
        set value (string split = $line)
        set key $value[1]
        set -e value[1]

        test $key = 'PATH'
        or set -gx $key $value
    end
end

test -f ~/.env.fish
and source ~/.env.fish
