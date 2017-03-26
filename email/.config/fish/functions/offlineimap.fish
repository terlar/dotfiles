function offlineimap
    set -l env_file ~/.local/share/offlineimap/env
    set -l cmd (type -fp offlineimap)
    env (cat $env_file) $cmd $argv
end
