function pastebin --description 'Paste something online'
  cat $argv ^/dev/null | while read -l line
    set input $input $line
  end

  if set -q input
    set url (echo $input | curl -ns -F 'f:1=<-' 'http://ix.io')
  else
    set url (curl -ns -F 'f:1=<-' 'http://ix.io' < $argv)
  end

  echo $url

  if which pbcopy >/dev/null ^/dev/null
    echo $url | pbcopy
  end
end
