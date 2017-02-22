# Commandline fzf
function __fzf_kb -e fish_user_key_bindings
    type -fq fzf
    or return

    type -q __fzf_reverse_isearch
    and bind \cr '__fzf_reverse_isearch'

    type -q __fzf_find_and_execute
    and bind \cx '__fzf_find_and_execute'

    type -q __fzf_find_file
    and bind \e'/' '__fzf_find_file'

    type -q __fzf_cd
    and bind \eg '__fzf_cd'

    type -q __fzf_cd_with_hidden
    and bind \eG '__fzf_cd_with_hidden'
end
