function fish_setup --description 'Setup fish variables'
    set -U fish_greeting

    # Set config path
    set -l config_home ~/.config
    if set -q XDG_CONFIG_HOME
        set config_home $XDG_CONFIG_HOME
    end
    set -U fish_config_path "$config_home/fish"

    # Bootstrap folders
    test -d "$fish_config_path/conf.d"
    or mkdir -p "$fish_config_path/conf.d"

    test -d "$fish_config_path/functions"
    or mkdir -p "$fish_config_path/functions"

    test -d "$fish_config_path/completions"
    or mkdir -p "$fish_config_path/completions"

    # Enable VI mode
    set -U fish_key_bindings fish_vi_key_bindings

    # Less colors
    set -Ux LESS_TERMCAP_mb \e'[01;31m' # begin blinking
    set -Ux LESS_TERMCAP_md \e'[01;38;5;75m' # begin bold
    set -Ux LESS_TERMCAP_so \e'[01;44m' # begin standout-mode - info box
    set -Ux LESS_TERMCAP_us \e'[04;38;5;200m' # begin underline
    set -Ux LESS_TERMCAP_me \e'[0m' # end mode
    set -Ux LESS_TERMCAP_se \e'[0m' # end standout-mode
    set -Ux LESS_TERMCAP_ue \e'[0m' # end underline

    # FZF Defaults
    set -Ux FZF_DEFAULT_OPTS --no-256
    set -Ux FZF_DEFAULT_COMMAND 'ag -l -g ""'

    # Colors
    fish_load_colors

    # Abbreviations
    fish_user_abbreviations

    # Completions
    fish_update_completions

    # Settings
    set -U tank_reporter spec
    set -U fry_auto_switch 1

    set -U fish_setup_done 1
    echo 'Initial fish setup done!'
end
