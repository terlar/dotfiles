set -Uq fish_setup_done; or fish_setup

function __prompt_context_reload -v __prompt_context_current
	echo $__prompt_context_current
end

# Plugins
set -l plugins_path (dirname (status -f))/plugins
for plugin in $plugins_path/*/autoload.fish
	source $plugin
end

# Farm
source /usr/local/share/fish-farm/farm.fish
# Fry
source /usr/local/share/fry/fry.fish

# Envoy
if type -fq envoy
	envoy -fp | source
end

# Secrets
source $HOME/.secrets
