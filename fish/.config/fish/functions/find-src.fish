function find-src
    set -l root (ghq root)
    set -l dir (ghq list $argv | fzf -0 -1)
    set -l path $root/$dir

    if test -n $dir -a -d $path
        cd $path
    else
        echo "find-src: no source directory matching $argv"
    end
end
