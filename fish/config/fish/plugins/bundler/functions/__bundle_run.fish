function __bundle_run --description 'Run file with bundler'
  if __bundler_installed
    if file_in_path Gemfile >/dev/null
      set argv bundle exec $argv
    end
  end

  eval command $argv
end
