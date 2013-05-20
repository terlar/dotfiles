function abbreviate --description 'Define a new abbreviation'
  if test (count $argv) -lt 2
    echo 'abbreviate: Takes two arguments. First abbreviation and then expanded command'
    return 1
  end

  abbreviations -e $argv[1]

  set -U fish_abbreviations $fish_abbreviations "$argv"
  return 0
end
