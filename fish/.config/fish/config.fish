set -Uq fish_setup_done
or fish_setup

if test "$TERM" = eterm-color
    function fish_title
    end
end

if type -q plug
    plug terlar/fry
    plug terlar/fish-farm
    plug joehillen/ev-fish
end

# Environment
if type -q ev
    ev auto >/dev/null
end
