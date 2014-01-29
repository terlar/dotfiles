set -q fish_setup_done; or fish_setup

function __prompt_context_reload -v __prompt_context_current
  echo $__prompt_context_current
end

# Plugins
set -l plugins_path (dirname (status -f))/plugins

# Bundler
source $plugins_path/bundler/bundler.fish
# Farm
source /usr/local/share/fish-farm/farm.fish
# Fry
source /usr/local/share/fry/fry.fish

# Envoy
if which envoy >/dev/null ^/dev/null
  eval (envoy -fp)
end
