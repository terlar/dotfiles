function git_prompt --description 'Write out the git prompt'
  __git_cache_clear

  if not __git_branch >/dev/null
    return
  end

  if not __git_status >/dev/null
    set_color $fish_color_git_clean
    __git_branch
    printf '✓'
    set_color normal
    return
  end

  if __git_staged
    set_color $fish_color_git_staged
  else
    set_color $fish_color_git_dirty
  end

  __git_branch
  printf '⚡'

  if __git_status_grep '^?? '
    set_color $fish_color_git_untracked
    printf '✭'
  end
  if __git_status_grep '^A  \|^M  '
    set_color $fish_color_git_added
    printf '✚'
  end
  if __git_status_grep '^ M \|^AM \|^RM \|^ T '
    set_color $fish_color_git_modified
    printf '*'
  end
  if __git_status_grep '^ D \|^AD '
    set_color $fish_color_git_deleted
    printf '✖'
  end
  if __git_status_grep '^R  \|^RM '
    set_color $fish_color_git_renamed
    printf '➜'
  end
  if __git_status_grep '^UU '
    set_color $fish_color_git_unmerged
    printf '═'
  end

  set_color normal
end
