set -Uq fish_setup_done; or fish_setup

# Scripts
set -l script_files \
	/usr/local/share/fish-farm/farm.fish \
	/usr/local/share/fry/fry.fish

for file in $script_files
	test -f $file; or continue
	source $file
end

# Environment
source $HOME/.env.fish
