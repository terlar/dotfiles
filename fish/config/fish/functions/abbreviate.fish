function abbreviate --description 'Define a new abbreviation'
  if test (count $argv) -lt 2
    echo 'abbreviate: Takes two arguments. First abbreviation and then expanded command'
    return 1
  end

  if abbreviations -q $argv[1]
    echo "abbreviate: Abbreviation '$argv[1]' already defined."
    return 1
  end

  set -U fish_abbreviations $fish_abbreviations "$argv"
  return 0
end
