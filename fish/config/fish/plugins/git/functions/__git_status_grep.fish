function __git_status_grep --description 'Grep in git status'
  if not set -q argv[1]
    return 1
  end

  __git_status | grep $argv[1] >/dev/null
  return $status
end
