function fry-use --description 'Use the ruby given by <name>'
  if test (count $argv) -eq 0
    echo 'fry-use: No <name> given.'
    echo
    echo 'Available rubies:'
    fry-rubies
    return 1
  end

  set -l name $argv[1]
  set -l ruby (fry-ls | grep -m 1 $name)

  if test -z "$ruby"
    set -l name (echo $name | sed 's/^ruby-//')
    set ruby (fry-ls | grep -m 1 $name)
  end

  if test -z "$ruby"
    echo "fry-use: Unknown ruby '$name' given."
    echo
    echo 'Available rubies:'
    fry-rubies
    return 1
  end

  if test (fry-current) = "$ruby"
    return 0
  end

  ln -sfn $fry_rubies/$ruby $fry_rubies/current
  echo "Switched to ruby '$ruby'"
end
