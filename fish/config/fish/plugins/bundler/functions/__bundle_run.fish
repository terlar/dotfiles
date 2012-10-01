function __bundle_run --description 'Run file with bundler'
  if __bundler_installed
    if test -n (project_file Gemfile)
      set argv bundle exec $argv
    end
  end

  echo $argv
end
