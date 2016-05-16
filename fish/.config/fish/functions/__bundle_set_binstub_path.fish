function __bundle_set_binstub_path --description 'Add binstub to path'
    if set -qg BUNDLE_BINSTUB_PATH
        return 1
    end

    set -l gemfile_path $argv[1]
    set -l binstub_path (dirname $gemfile_path)/bin

    if test -d $binstub_path
        set -xg BUNDLE_BINSTUB_PATH $binstub_path

        set -l ruby_path (dirname (type -p ruby))
        set -l pos (contains -i $ruby_path $fish_user_paths)
        if test $status -eq 0
            set fish_user_paths $fish_user_paths[1..$pos $pos..-1]
            set fish_user_paths[$pos] $binstub_path
        else
            set fish_user_paths $fish_user_paths $binstub_path
        end
    end
end
