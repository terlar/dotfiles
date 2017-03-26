function vdirsyncer
    set -l env_file ~/.local/share/vdirsyncer/env
    set -l cmd (type -fp vdirsyncer)
    env (cat $env_file) $cmd $argv
end
