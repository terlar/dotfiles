function __git_cache_clear --description 'Clear git cache'
  set -e __git_cache_branch
  set -e __git_cache_status
  set -e __git_cache_staged
end
