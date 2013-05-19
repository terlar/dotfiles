function abbreviations --description 'List, show and query abbreviations'
  if test (count $argv) = 0
    printf '%s\n' $fish_abbreviations
    return
  end

  set -l expanded_abbreviation

  for i in $fish_abbreviations
    echo $i | read -l abbreviation command
    if test $abbreviation = $argv[-1]
      set expanded_abbreviation $command
    end
  end

  if test -n "$expanded_abbreviation"
    switch $argv[1]
      case -q --query
        return 0
      case '*'
        echo $expanded_abbreviation
      end
  else
    return 1
  end
end
