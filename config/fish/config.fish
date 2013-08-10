set -q fish_setup_done; or fish_setup

function -v __prompt_context_current __prompt_context_reload
  echo $__prompt_context_current
end

# Plugins
set -l plugins_path (dirname (status -f))/plugins

# Bundler
. $plugins_path/bundler/bundler.fish
# Farm
. /usr/local/share/fish-farm/farm.fish
# Fry
. /usr/local/share/fry/fry.fish
