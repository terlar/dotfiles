function z --description 'Start zeus or run command'
  if not which zeus >/dev/null
    echo "fish: Unknown command 'z'"
    return 1
  end

  if test (count $argv) = 0
    if not test -f zeus.json
      zeus init
    end
    zeus start
  else
    zeus $argv
  end
end
