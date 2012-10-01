function bgem
  if count $argv >/dev/null
    switch $argv[1]
      case 'unlock'
        test -n "$BUNDLE_GEMFILE"
        and cp 'Gemfile.lock' "$BUNDLE_GEMFILE.lock"
      case 'off'
        set -e BUNDLE_GEMFILE
      case '/*'
        test -f $argv[1]
        and set -g BUNDLE_GEMFILE $argv[1]
      case '*'
        test -f $argv[1]
        and set -g BUNDLE_GEMFILE $PWD/$argv[1]
    end
  else
    if test -n "$BUNDLE_GEMFILE"
      echo "Using Bundle Gemfile ($BUNDLE_GEMFILE)"
    else
      echo 'Bundle Gemfile not set'
    end
  end
end
