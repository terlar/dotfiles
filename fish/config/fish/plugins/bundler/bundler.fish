# Initialize
set -l bundler_path (dirname (status -f))

if not contains $bundler_path/functions $fish_function_path
  set fish_function_path $bundler_path/functions $fish_function_path
end

if not contains $bundler_path/completions $fish_complete_path
  set fish_complete_path $bundler_path/completions $fish_complete_path
end


# Alias
function b    ; bundle_unlocked $argv; end
function bgem ; bundle_gemfile $argv; end


# Toggle binstub path
function __bundler_set_binstub_path --on-event fish_prompt
  set -l gemfile_path (file_in_path Gemfile)

  if test -z $gemfile_path
    __bundle_reset_binstub_path
  else
    not test -z $bundle_binstub_path; and return

    set -xg bundle_binstub_path (dirname $gemfile_path)/bin
    not test -d $bundle_binstub_path; and return

    set PATH $bundle_binstub_path $PATH
  end
end

# Toggle local gemfile
function __bundler_toggle_local_gemfile --on-variable PWD
  status --is-command-substitution; and return

  set -l gemfile Gemfile.local
  set -l gemfile_path (file_in_path $gemfile)

  if test -z $gemfile_path
    bundle_gemfile off
  else
    not test -z $BUNDLE_GEMFILE; and return

    bundle_gemfile $gemfile_path
    bundle_gemfile
  end
end
