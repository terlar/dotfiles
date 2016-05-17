function __pisces_kb -e fish_user_key_bindings
    type -q _pisces_bind_pair
    or return

    set -q pisces_pairs
    or set -U pisces_pairs '(,)' '[,]' '{,}' '","' "','"

    set -q pisces_mode
    or set -U pisces_mode '-M insert'

    for pair in $pisces_pairs
        _pisces_bind_pair (string split ',' $pair)
    end

    # normal backspace, also known as \010 or ^H:
    bind $pisces_mode \b _pisces_backspace
    # Terminal.app sends DEL code on ⌫:
    bind $pisces_mode \177 _pisces_backspace

    # overrides TAB to provide completion of vars before a closing '"'
    bind $pisces_mode \t _pisces_complete
end
