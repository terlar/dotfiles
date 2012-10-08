function bundle_gemfile --description 'Manage bundle gemfile'
  if test (count $argv) = 0
    if test -z $BUNDLE_GEMFILE
      echo 'Bundle Gemfile not set'
    else
      echo "Using Bundle Gemfile ($BUNDLE_GEMFILE)"
    end
    return
  end

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
end
