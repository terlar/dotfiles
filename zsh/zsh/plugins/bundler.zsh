_bundle_gemfile_prompt() {
  if [ $BUNDLE_GEMFILE ]; then
    echo "$BUNDLE_GEMFILE_PROMPT_PREFIX${BUNDLE_GEMFILE##*/}$BUNDLE_GEMFILE_PROMPT_SUFFIX"
  fi
}

_bundler-installed() {
  which bundle > /dev/null 2>&1
}

_within-bundled-project() {
  [ "$(_file-within-project Gemfile)" != "" ]
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
      "unlock")
        [ $BUNDLE_GEMFILE ] && cp "Gemfile.lock" "$BUNDLE_GEMFILE.lock";;
      "off")
        unset BUNDLE_GEMFILE;;
      /*)
        [ -f "$1" ] && export BUNDLE_GEMFILE=$1;;
      *)
        [ -f "$1" ] && export BUNDLE_GEMFILE="$PWD/$1";;
    esac
  fi
}
alias bgem='bundle_gemfile'
zstyle ':completion:*:bundle_gemfile:*' ignored-patterns '(*/)#Gemfile' '(*/)#*.lock'

# Unlock Gemfile and run bundle
unlocked_bundle() {
  bundle_gemfile unlock
  if [ $# -eq 0 ]; then
    bundle
  else
    bundle "$*"
  fi
}
alias b='unlocked_bundle'
compdef _bundle unlocked_bundle=bundle

# Toggle local Gemfile
_toggle-local-gemfile() {
  local gemfile="Gemfile.local"
  local gemfile_path=$(_file-within-project $gemfile)

  if [ "$gemfile_path" != "" ]; then
    [ $BUNDLE_GEMFILE ] && return
    bundle_gemfile $gemfile_path
  else
    bundle_gemfile off
  fi
}
chpwd_functions+=(_toggle-local-gemfile)
