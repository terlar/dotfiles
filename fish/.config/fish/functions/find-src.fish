function find-src
    set -l root (ghq root)
    set -l path

    if test -d "$root/$argv"
        set path "$root/$argv"
    else
        set -l dir (ghq list "$argv" | fzf -0 -1)
        if test -n "$dir" -a -d "$root/$dir"
            set path "$root/$dir"
        end
    end

    if test -n "$path"
        cd $path
    else
        echo "find-src: no source directory matching: `$argv'"
    end
end
