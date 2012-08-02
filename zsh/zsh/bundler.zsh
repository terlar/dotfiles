_bundler-installed() {
  which bundle > /dev/null 2>&1
}

_within-bundled-project() {
  local check_dir=$PWD
  while [ $check_dir != "/" ]; do
    [ -f "$check_dir/Gemfile" ] && return
    check_dir="$(dirname $check_dir)"
  done
  false
}

_run-with-bundler() {
  if _bundler-installed && _within-bundled-project; then
    bundle exec $@
  else
    $@
  fi
}

bundled_commands=(cucumber guard rails rake rspec ruby spec spork)
for cmd in $bundled_commands; do
  eval "function bundled_$cmd () { _run-with-bundler $cmd \$@}"
  alias $cmd=bundled_$cmd

  if which _$cmd > /dev/null 2>&1; then
    compdef _$cmd bundled_$cmd=$cmd
  fi
done

# Use local Gemfile
LOCAL_GEMFILE="Gemfile.local"
local_gemfile() {
  if [ $# -eq 0 ]; then
    if [ "$BUNDLE_GEMFILE" = "$LOCAL_GEMFILE" ]; then
      echo "Local Gemfile is ON ($LOCAL_GEMFILE)"
    else
      echo "Local Gemfile is OFF"
    fi
  else
    case $1 in
      "on" )
        export BUNDLE_GEMFILE=$LOCAL_GEMFILE;;
      "off" )
        unset BUNDLE_GEMFILE;;
    esac
  fi
}

chpwd() {
  if [ -f "$LOCAL_GEMFILE" ]; then
    local_gemfile on
  else
    local_gemfile off
  fi
}
