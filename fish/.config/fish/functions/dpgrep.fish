function dpgrep
    argparse a/list-full -- $argv
    if set -ql _flag_list_full
        docker ps --filter name="$argv"
    else
        docker ps -q --filter name="$argv"
    end
    echo hello world
end
