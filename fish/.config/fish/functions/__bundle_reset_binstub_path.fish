function __bundle_reset_binstub_path --description 'Remove binstub from path'
    set -qg BUNDLE_BINSTUB_PATH; or return
    set -l new_path

    for i in $fish_user_paths
        if test $i != $BUNDLE_BINSTUB_PATH
            set new_path $new_path $i
        end
    end

    set -g fish_user_paths $new_path
    set -eg BUNDLE_BINSTUB_PATH
end

