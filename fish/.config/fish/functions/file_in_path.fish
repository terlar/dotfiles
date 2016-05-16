function file_in_path --description 'Find file in path'
    set -l file $argv[1]
    set -l dir $PWD

    while test $dir != '/'
        if test -f $dir/$file
            echo -n $dir/$file
            return 0
        end
        set dir (dirname $dir)
    end

    return 1
end
