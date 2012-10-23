# Alias
function b    ; bundle_unlocked $argv; end
function bgem ; bundle_gemfile $argv; end


# Automatic bundle exec
for cmd in ruby rails rake rspec spec cucumber spork
  eval "function $cmd; __bundle_run $cmd \$argv; end"
end


# Toggle local gemfile
function __bundler_toggle_local_gemfile --on-variable PWD
  if status --is-command-substitution
    return
  end

  set -l gemfile Gemfile.local
  set -l gemfile_path (file_in_path $gemfile)

  if test -z $gemfile_path
    bundle_gemfile off
  else
    if test -z $BUNDLE_GEMFILE
      bundle_gemfile $gemfile_path
      bundle_gemfile
    end
  end
end
