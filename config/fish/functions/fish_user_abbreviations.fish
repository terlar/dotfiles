function fish_user_abbreviations
  abbreviate !    'sudo'
  abbreviate tf   'tail -f'
  abbreviate f    'farm'
  abbreviate fcd  'farm cd'

  # ls
  abbreviate l    'ls -a'
  abbreviate l.   'ls -d .*'

  # git
  abbreviate g    'git'
  abbreviate gs   'git status -sb'

  abbreviate gh   'git head'
  abbreviate gl   'git l'
  abbreviate gll  'git ll'
  abbreviate gwc  'git wc'

  abbreviate gd   'git diff'
  abbreviate gdt  'git difftool'
  abbreviate gdc  'git diff --cached'
  abbreviate gdw  'git diff --color-words'
  abbreviate gds  'git diff --stat'

  abbreviate ga   'git add'
  abbreviate gc   'git commit -v'
  abbreviate gca  'git commit -v -a'

  abbreviate gb   'git branch'
  abbreviate gco  'git checkout'

  abbreviate gf   'git fetch'
  abbreviate gfa  'git fetch --all'
  abbreviate gm   'git merge'
  abbreviate gr   'git rebase'
  abbreviate gcp  'git cherry-pick'

  # ruby
  abbreviate rc   'rails console'
  abbreviate rg   'rails generate'
  abbreviate be   'bundle exec'
  abbreviate s    'spring'

  # tools
  abbreviate kc 'kviberg-config'
end
