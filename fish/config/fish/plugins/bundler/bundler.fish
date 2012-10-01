# Prompt
function __misc_prompt_bundler --on-event misc_prompt
  set -g misc_prompt (bundler_prompt) $misc_prompt
end

# Automatic bundle exec
set -U __bundle_commands ruby rails rake rspec spec cucumber spork
for cmd in $__bundle_commands
  alias $cmd (__bundle_run $cmd)
end

# Toggle local gemfile
function __bundle_toggle_local_gemfile --on-variable PWD
  if status --is-command-substitution
    return
  end

  set -l gemfile Gemfile.local
  set -l gemfile_path (project_file $gemfile)

  if test -z $gemfile_path
    bundle_gemfile off
  else
    if test -z $BUNDLE_GEMFILE
      bundle_gemfile $gemfile_path
    end
  end
end

function b    ; bundle_unlocked $argv; end
function bgem ; bundle_gemfile $argv; end
