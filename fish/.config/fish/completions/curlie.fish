complete --command curlie \
    --no-files \
    --condition '__fish_is_nth_token 1' \
    --description 'HTTP method' \
    --arguments 'GET HEAD POST PUT DELETE CONNECT OPTIONS TRACE PATCH'

complete --command curlie --wraps curl
