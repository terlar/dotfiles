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

if not pgrep gpg-agent >/dev/null
    and test -x /usr/local/bin/pinentry-mac
    gpg-agent --daemon --pinentry-program /usr/local/bin/pinentry-mac
end
