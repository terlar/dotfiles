function jcurl
    curl -s -H 'Content-Type: application/json' $argv | jq -C . | less -R
end
