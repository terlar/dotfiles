function bundle_gemfile --description 'Manage bundle gemfile'
  if count $argv >/dev/null
    switch $argv[1]
      case 'unlock'
        not test -z $BUNDLE_GEMFILE
        and cp Gemfile.lock $BUNDLE_GEMFILE.lock
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
    if test -z $BUNDLE_GEMFILE
      echo 'Bundle Gemfile not set'
    else
      echo "Using Bundle Gemfile ($BUNDLE_GEMFILE)"
    end
  end
end
