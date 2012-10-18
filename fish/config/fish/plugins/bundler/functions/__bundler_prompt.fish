function __bundler_prompt --description 'Write out the bundler prompt'
  not test -z $BUNDLE_GEMFILE
  and echo -n (basename $BUNDLE_GEMFILE)
end
