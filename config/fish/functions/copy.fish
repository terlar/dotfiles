function copy --description 'Trim new lines and copy toclipboard'
  cat $argv ^/dev/null | tr -d '\n' | read -l input

  set -ql input; or set -l input $argv

  if test -n "$input"
    echo $input | tr -d '\n' | pbcopy
  end
end
