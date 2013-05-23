function fish_user_abbreviations
  abbreviate !    'sudo'
  abbreviate tf   'tail -f'
  abbreviate f    'farm'
  abbreviate fcd  'farm cd'

  # ls
  abbreviate l    'ls -la'
  abbreviate l.   'ls -d .*'

  # git
  abbreviate g    'git'
  abbreviate gs   'git status'

  abbreviate gl   'git l'
  abbreviate gls  'git ls'
  abbreviate gwc  'git wc'

  abbreviate gd   'git diff'
  abbreviate gdw  'git diff --color-words'
  abbreviate gds  'git diff --stat'
  function gdv   ; git diff -w $argv | view - ; end

  abbreviate ga   'git add'
  abbreviate gc   'git commit -v'
  abbreviate gca  'git commit -v -a'

  abbreviate gb   'git branch'
  abbreviate gco  'git checkout'

  abbreviate gf   'git fetch'
  abbreviate gm   'git merge'
  abbreviate gr   'git rebase'

  # ruby
  abbreviate rc   'rails console'
  abbreviate rg   'rails generate'
  abbreviate be   'bundle exec'
  abbreviate s    'spring'

  # tools
  abbreviate kc 'kviberg-config'
end
