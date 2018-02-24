# Toggle binstub path
function __bundler_set_binstub_path -v PWD
    set -l gemfile_path (file_in_path Gemfile)

    test -e "$gemfile_path"
    or return

    set -l binstub_path (string replace /Gemfile '' $gemfile_path)/bin

    if test -e "$binstub_path"
        test "$__bundler_binpath" = "$binstub_path"
        and return

        set -g __bundler_binpath "$binstub_path"
        set -g fish_user_paths $fish_user_paths $__bundler_binpath
    else
        set -q __node_binpath
        and set -l index (contains -i -- "$__bundler_binpath" $fish_user_paths)
        and set -e fish_user_paths[$index]
        and set -e __bundler_binpath
    end
end

__bundler_set_binstub_path $PWD
