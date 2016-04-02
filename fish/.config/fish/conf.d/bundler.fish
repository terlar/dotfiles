# Toggle binstub path
function __bundler_set_binstub_path --on-event fish_prompt
	set -l gemfile_path (file_in_path Gemfile)

	if test -z $gemfile_path
		__bundle_reset_binstub_path
	else
		__bundle_set_binstub_path $gemfile_path
	end
end
