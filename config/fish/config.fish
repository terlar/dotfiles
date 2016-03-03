set -Uq fish_setup_done; or fish_setup

set -g fish_user_paths $HOME/.local/bin

# Plugins
set -l plugins_path (dirname (status -f))/plugins
for plugin in $plugins_path/*/autoload.fish
	source $plugin
end

# Scripts
set -l script_files \
	/usr/local/share/fish-farm/farm.fish \
	/usr/local/share/fry/fry.fish \
	/usr/share/fish/functions/fzf.fish

for file in $script_files
	test -f $file; or continue
	source $file
end

# Secrets
source $HOME/.secrets
