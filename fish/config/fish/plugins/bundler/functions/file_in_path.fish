function file_in_path --description 'Find file in path'
  set -l file $argv[1]
  set -l check_dir $PWD

  while test $check_dir != '/'
    if test -f $check_dir/$file
      echo -n $check_dir/$file
      return 0
    end
    set check_dir (dirname $check_dir)
  end

  return 1
end
