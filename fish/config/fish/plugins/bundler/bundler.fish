# Prompt
function __misc_prompt_bundler --on-event misc_prompt
  set -g misc_prompt (bundler_prompt) $misc_prompt
end


# Autocd
function __toggle_local_gemfile --on-variable PWD
  if status --is-command-substitution
    return
  end

  set -l gemfile Gemfile.local
  set -l gemfile_path (project_file $gemfile)

  if test -n "$gemfile_path"
    test -n "$BUNDLE_GEMFILE"; and return
    bgem $gemfile_path
  else
    bgem off
  end
end


# Automatic bundle exec
function __bundler_installed
  which bundle > /dev/null 2>&1
end

function __bundle_run
  if __bundler_installed
    if test -n (project_file Gemfile)
      set argv bundle exec $argv
    end
  end

  echo $argv
end

set -U __bundle_commands ruby rails rake rspec spec cucumber spork
for cmd in $__bundle_commands
  alias $cmd (__bundle_run $cmd)
end
