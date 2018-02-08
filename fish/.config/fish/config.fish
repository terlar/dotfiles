set -Uq fish_setup_done
or fish_setup

if test "$TERM" = eterm-color
    function fish_title
    end
end

if type -q plug
    plug fisherman/fzf
    plug terlar/ev-fish
    plug terlar/fish-farm
    plug terlar/fry
end

# Environment
if type -q ev
    ev auto >/dev/null
end
