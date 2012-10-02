function __git_staged --description 'Get git staged status (cached)'
  if not set -q __git_cache_staged
    set -g __git_cache_staged (git diff --name-only --cached 2>/dev/null)
  end

  test -n "$__git_cache_staged"
end
