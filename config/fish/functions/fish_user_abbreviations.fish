function fish_user_abbreviations
  set -U fish_user_abbreviations \
    '!=sudo' \
    'tf=tail -f' \
    'f=farm' \
    'fcd=farm cd' \
    'l=ls -a' \
    'l.=ls -d .*'\
    'g=git'\
    'gs=git status -sb'\
    'gh=git head'\
    'gl=git l'\
    'gll=git ll'\
    'gwc=git wc'\
    'gd=git diff'\
    'gdt=git difftool'\
    'gdc=git diff --cached'\
    'gdw=git diff --color-words'\
    'gds=git diff --stat'\
    'ga=git add'\
    'gc=git commit -v'\
    'gca=git commit -v -a'\
    'gb=git branch'\
    'gco=git checkout'\
    'gf=git fetch'\
    'gfa=git fetch --all'\
    'gm=git merge'\
    'gr=git rebase'\
    'gcp=git cherry-pick'\
    'b=bundle'\
    'rc=farm console'\
    'rs=farm server'\
    'be=bundle exec'\
    's=spring'\
    'kc=kviberg-config'
end
