set -q fish_setup_done; or fish_setup

# Plugins
set -l plugins_path (dirname (status -f))/plugins

# Bundler
. $plugins_path/bundler/bundler.fish
# Farm
. /usr/local/share/fish-farm/farm.fish
# Fry
. /usr/local/share/fry/fry.fish
