function pastebin --description 'Paste something online'
  cat $argv ^/dev/null | while read -l line
    echo $line
  end | curl -ns -F 'f:1=<-' 'http://ix.io' | read -l url

  echo $url

  if which pbcopy >/dev/null ^/dev/null
    echo $url | pbcopy
  end
end
