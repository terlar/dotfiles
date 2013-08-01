function abbr --description 'Define a new abbreviation'
  if test (count $argv) -lt 2
    echo 'abbr: Takes two arguments. First abbreviation and then expanded command'
    return 1
  end

  echo $argv | read -l abbreviation command

  for i in $fish_user_abbreviations
    switch $i
      case "$abbreviation=*"
        echo $i
        break
    end
  end | read -l old_abbreviation

  if set -q old_abbreviation
    set -l idx (contains -i $old_abbreviation $fish_user_abbreviations)
    set -e fish_user_abbreviations[$idx]
  end

  set fish_user_abbreviations $fish_user_abbreviations "$abbreviation=$command"
end
