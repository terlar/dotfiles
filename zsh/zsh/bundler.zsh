_bundler-installed() {
  which bundle > /dev/null 2>&1
}

_within-bundled-project() {
  _file-within-project("Gemfile")
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

bundle_gemfile() {
  if [ $# -eq 0 ]; then
    if [ $BUNDLE_GEMFILE ]; then
      echo "Using Bundle Gemfile ($BUNDLE_GEMFILE)"
    else
      echo "Bundle Gemfile not set"
    fi
  else
    case $1 in
      "off") unset BUNDLE_GEMFILE;;
      *) export BUNDLE_GEMFILE=$1;;
    esac
  fi
}

# Use local Gemfile
chpwd() {
  local gemfile="Gemfile.local"
  local gemfile_path=$(_file-within-project $gemfile)

  if [ "$gemfile_path" != "" ]; then
    bundle_gemfile $gemfile_path
    bundle_gemfile
  else
    bundle_gemfile off
  fi
}
