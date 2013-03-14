# Path
set -gx fry_rubies $HOME/.rubies
set PATH $fry_rubies/current/bin $PATH

# Auto-switch
function __fry_auto_switch --on-variable PWD --description 'Auto-switch ruby version from .ruby-version file'
  status --is-command-substitution; and return
  test -f .ruby-version; or return

  echo 'There is a .ruby-version'
end
