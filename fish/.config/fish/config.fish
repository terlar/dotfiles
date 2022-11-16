set -Uq fish_setup_done
or fish_setup

if test "$TERM" = eterm-color
    function fish_title
    end
end

# Add marker to support navigation between prompts.
function mark_prompt_start --on-event fish_prompt
    echo -en "\e]133;A\e\\"
end

# Environment
if type -q ev
    ev auto >/dev/null
end
