function __bundle_set_binstub_path --description 'Add binstub to path'
  if set -qg bundle_binstub_path
    return 1
  end

  set -l gemfile_path $argv[1]

  set -xg bundle_binstub_path (dirname $gemfile_path)/bin
  if test -d $bundle_binstub_path
    set PATH $bundle_binstub_path $PATH
  end
end
