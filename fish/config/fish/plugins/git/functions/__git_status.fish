function __git_status --description 'Get git status (cached)'
  if not set -q __git_cache_status
    set -g __git_cache_status (git status --porcelain 2>/dev/null)
  end

  if test -z "$__git_cache_status"
    return 1
  end

  for row in $__git_cache_status
    echo $row
  end
  return 0
end
