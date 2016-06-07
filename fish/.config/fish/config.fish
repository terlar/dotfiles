set -Uq fish_setup_done
or fish_setup

if test "$TERM" = eterm-color
    function fish_title
    end
end

plug terlar/fry
plug terlar/fish-farm

# Environment
source ~/.env.fish
