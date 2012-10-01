function __bundle_run --description 'Run file with bundler'
  if __bundler_installed
    if file_in_path Gemfile >/dev/null
      bundle exec $argv
      return $status
    end
  end

  set -l cmd (which $argv[1])
  set -e argv[1]

  if test -z $cmd
    return 1
  end

  eval $cmd $argv
end
