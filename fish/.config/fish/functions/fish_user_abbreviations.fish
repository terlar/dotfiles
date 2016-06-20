function fish_user_abbreviations
    set -U fish_user_abbreviations

    abbr tf 'tail -f'
    abbr l 'ls -a'
    abbr l. 'ls -d .*'
    abbr m mux
    abbr e editor
    abbr week "date '+%V'"

    if type -fq systemctl
        abbr j 'journalctl --since=today'
        abbr je 'journalctl --since=today --priority=0..3'
        abbr jb 'journalctl -b'
        abbr jf 'journalctl -f'
        abbr ju 'journalctl -u'
        abbr juu 'journalctl --user-unit'

        abbr sc 'systemctl'
        abbr scu 'systemctl --user'
        abbr scs 'systemctl status'
        abbr scl 'systemctl list-units'

        abbr reboot 'systemctl reboot'
        abbr poweroff 'systemctl poweroff'
        abbr suspend 'systemctl suspend'
    end

    if type -fq git
        abbr g 'git'
        abbr gb 'git branch'
        abbr gba 'git bactive'
        abbr gs 'git status -sb'

        abbr gco 'git checkout'
        abbr gf 'git fetch'
        abbr gfa 'git fetchall'
        abbr gsmr 'git-submodule-reset'
        abbr gsmp 'git subpull'

        abbr gh 'git head'
        abbr gl 'git l'
        abbr gll 'git ll'
        abbr gwc 'git wc'

        abbr gd 'git diff'
        abbr gdt 'git difftool'
        abbr gdc 'git diff --cached'
        abbr gdw 'git diff --color-words'
        abbr gds 'git diff --stat'

        abbr ga 'git add'

        abbr gc 'git commit'
        abbr gca 'git commit -a'
        abbr gcp 'git cherry-pick'
        abbr gr 'git rebase'

        abbr gm 'git merge'
        abbr gpr 'git-pull-request'

        abbr gst 'git stash'
        abbr gsts 'git stash show -p'
        abbr gsta 'git stash apply'
    end

    if type -fq pacman
        abbr pac 'pacman'
    end

    if type -fq aurget
        abbr aur 'aurget'
    end

    if type -fq pygmentize
        abbr ccat pygmentize
    end

    if type -fq bundle
        abbr b bundle
        abbr be 'bundle exec'
    end

    if type -fq docker
        abbr d docker
        abbr dim 'docker images'
        abbr dp 'docker ps'
        abbr dpa 'docker ps -a'
        abbr dpq 'docker ps -q'
        abbr drmc 'docker rm -v (docker ps -qa --filter status=exited)'
        abbr drmi 'docker rmi (docker images -q --filter dangling=true)'
    end

    if type -q farm
        abbr f 'farm'
        abbr fcd 'farm cd'
    end

    if type -q saltside-workstation
        abbr sw saltside-workstation
    end
end
