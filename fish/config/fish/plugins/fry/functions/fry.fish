function fry --description 'Fishy ruby switcher'
  if test (count $argv) -eq 0
    # Default command
    set argv rubies
  end

  set -l command $argv[1]
  set -e argv[1]
  set -l func_name "fry-$command"

  if functions -q $func_name
    eval $func_name $argv
  else
    # Missing command
    fry-help
  end

  return $status
end
