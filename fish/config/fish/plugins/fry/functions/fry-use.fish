function fry-use --description 'Use the ruby given by <ruby>'
  if test (count $argv) -eq 0
    echo 'fry-use: No <ruby> given'
    echo
    echo 'Available rubies:'
    fry-rubies
    return 1
  end

  set -l name $argv[1]

  if test $name = 'system'
    fry-reset
    echo 'Switched to system ruby'
    return 0
  end

  set -l ruby (fry-ls | grep -m 1 $name)

  if test -z "$ruby"
    set -l name (echo $name | sed 's/^ruby-//')
    set ruby (fry-ls | grep -m 1 $name)
  end

  if test -z "$ruby"
    echo "fry-use: Unknown ruby '$name'"
    echo
    echo 'Available rubies:'
    fry-rubies
    return 1
  end

  if test (fry-current) = "$ruby"
    return 0
  end

  fry-reset
  set PATH $fry_rubies/$ruby/bin $PATH
  echo "Switched to ruby '$ruby'"
end
