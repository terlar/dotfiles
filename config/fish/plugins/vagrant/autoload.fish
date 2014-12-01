# Initialize
set -l plugin_path (dirname (status -f))

if not contains $plugin_path/completions $fish_complete_path
	set fish_complete_path $plugin_path/completions $fish_complete_path
end
