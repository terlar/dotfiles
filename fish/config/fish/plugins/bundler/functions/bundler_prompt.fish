function bundler_prompt --description 'Write out the bundler prompt'
  if test -n "$BUNDLE_GEMFILE"
    printf (basename $BUNDLE_GEMFILE)
  end
end
