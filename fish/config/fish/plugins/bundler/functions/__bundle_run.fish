function __bundle_run --description 'Run file with bundler'
  if which bundle >/dev/null
    if file_in_path Gemfile >/dev/null
      set argv bundle exec $argv
    end
  end

  if not which $argv[1] >/dev/null
    echo "fish: Unknown command '$argv[1]'"
    return 1
  end

  eval command $argv
end
