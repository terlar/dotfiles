set -Uq fish_setup_done
or fish_setup

if test "$TERM" = eterm-color
    function fish_title
    end
end

# Environment
if type -q ev
    ev auto >/dev/null
end
