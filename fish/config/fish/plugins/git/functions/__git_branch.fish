function __git_branch --description 'Get git branch (cached)'
  if not set -q __git_cache_branch
    set -g __git_cache_branch (git symbolic-ref HEAD 2>/dev/null | cut -f3 -d '/')
  end

  if test -z $__git_cache_branch
    return 1
  end

  printf $__git_cache_branch
  return 0
end
