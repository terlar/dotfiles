function abbreviate --description 'Creates an abbreviation'
  set -U fish_abbreviations $fish_abbreviations "$argv"
end
