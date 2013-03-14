# Path
set -gx fry_rubies $HOME/.rubies

if test -f $HOME/.ruby-version
  fry use (cat $HOME/.ruby-version)
end

# Auto-switch
function __fry_auto_switch --on-variable PWD --description 'Auto-switch ruby version from .ruby-version file'
  status --is-command-substitution; and return
  set -q fry_auto_switch; or return

  set -l version_file (file_in_path .ruby-version)
  test -z "$version_file"; and return

  fry use (cat $version_file)
end
