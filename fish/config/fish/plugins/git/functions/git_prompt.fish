function git_prompt --description 'Write out the git prompt'
  set -l branch (git symbolic-ref HEAD 2> /dev/null | cut -f3 -d '/')
  if test -z "$branch"
    return
  end
  set -l index (git status --porcelain 2> /dev/null)

  if test -z "$index"
    set_color $fish_color_git_clean
    printf $branch
    printf '✓'
    set_color normal
    return
  end

  set -l staged (git diff --name-only --cached)
  if test -n "$staged"
    set_color $fish_color_git_staged
  else
    set_color $fish_color_git_dirty
  end

  printf $branch
  printf '⚡'

  if test (echo $index | grep '^?? ')
    set_color $fish_color_git_untracked
    printf '✭'
  end
  if test (echo $index | grep '^A  \|^M  ')
    set_color $fish_color_git_added
    printf '✚'
  end
  if test (echo $index | grep '^ M \|^AM \|^ T ')
    set_color $fish_color_git_modified
    printf '*'
  end
  if test (echo $index | grep '^ D \|^AD ')
    set_color $fish_color_git_deleted
    printf '✖'
  end
  if test (echo $index | grep '^R  ')
    set_color $fish_color_git_renamed
    printf '➜'
  end
  if test (echo $index | grep '^UU ')
    set_color $fish_color_git_unmerged
    printf '═'
  end

  set_color normal
end
