function prompt_bundler --description 'Write out the bundler prompt'
  not test -z $BUNDLE_GEMFILE
  and printf (basename $BUNDLE_GEMFILE)
end