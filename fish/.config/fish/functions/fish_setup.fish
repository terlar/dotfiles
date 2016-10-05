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

    # FZF Defaults
    set -Ux FZF_DEFAULT_OPTS --no-256
    set -Ux FZF_DEFAULT_COMMAND 'ag --hidden -l -g ""'

    # Colors
    fish_load_colors

    # Abbreviations
    fish_user_abbreviations

    # Completions
    fish_update_completions

    # Settings
    set -U fish_escape_delay_ms 10

    set -U tank_reporter spec
    set -U fry_auto_switch 1

    set -U fish_setup_done 1
    echo 'Initial fish setup done!'
end
