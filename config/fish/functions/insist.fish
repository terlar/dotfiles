function insist
  eval $argv ^/dev/null
  set -l code $status

  if test $code -eq 0
    return 0
  else if test $code -eq 127
    return $code
  else
    echo 'insist: Unsuccessful, trying again...'
    sleep 0.5
    eval insist $argv
  end
end
