function fish_user_abbreviations
    set -U fish_user_abbreviations

    # Aliases
    abbr @ 'head'
    abbr tf 'tail -f'
    abbr l 'ls -a'
    abbr l. 'ls -d .*'
    abbr m mux
    abbr e editor
    abbr day "date '+%d (%A)'"
    abbr week "date '+%V'"
    abbr month "date '+%m (%B)'"

    # Math
    abbr -- '+' 'add'
    abbr -- '-' 'sub'
    abbr -- '*' 'mul'
    abbr -- '/' 'div'

    # Command defaults
    abbr time "time -p"
    abbr base64 "base64 -w0"

    if type -fq tree
        abbr tree 'tree -a'
    end

    if type -fq rg
        abbr ag 'rg -S --hidden --glob "!.git/*"'
        abbr rg 'rg -S --hidden --glob "!.git/*"'
    end

    if type -fq systemctl
        abbr j 'journalctl --since=today'
        abbr je 'journalctl --since=today --priority=0..3'
        abbr jb 'journalctl --boot'
        abbr jf 'journalctl --follow'
        abbr ju 'journalctl --unit'
        abbr juu 'journalctl --user-unit'

        abbr sc 'systemctl'
        abbr scs 'systemctl status'
        abbr scl 'systemctl list-units'

        abbr scu 'systemctl --user'
        abbr scus 'systemctl --user status'
        abbr scul 'systemctl --user list-units'

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
        abbr glll 'git lll'
        abbr gwc 'git wc'

        abbr gd 'git diff'
        abbr gdt 'git difftool'
        abbr gdc 'git diff --cached'
        abbr gdw 'git diff --color-words'
        abbr gds 'git diff --stat'

        abbr ga 'git add'
        abbr gap 'git add -Ap'

        abbr gc 'git commit'
        abbr gca 'git commit -a'
        abbr gcm 'git commit -m'
        abbr gcw 'git commit -m WIP'
        abbr gcp 'git cherry-pick'
        abbr gr 'git rebase'

        abbr gm 'git merge'
        abbr gpr 'git-pull-request'
        abbr gpf 'git push --force-with-lease'

        abbr gst 'git stash'
        abbr gsts 'git stash show -p'
        abbr gsta 'git stash apply'

        abbr guns 'git reset HEAD'
        abbr gunc 'git reset --soft HEAD^'
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

    if type -fq docker
        abbr d docker
        abbr dim 'docker images'

        abbr dp 'docker ps'
        abbr dpa 'docker ps -a'
        abbr dpq 'docker ps -q'

        abbr drmc 'docker rm -v (docker ps -qa --filter status=exited)'
        abbr drmca 'docker rm -fv (docker ps -qa)'
        abbr drmi 'docker rmi (docker images -q --filter dangling=true)'
    end

    if type -fq docker-compose
        abbr dc docker-compose
        abbr dcl docker-compose logs
    end

    if type -fq kubectl
        abbr kb kubectl
        abbr kbg 'kubectl get'
        abbr kbd 'kubectl describe'
        abbr kbl 'kubectl logs'
    end

    if type -fq ghq
        abbr f 'ghq look'
        abbr gg 'ghq look'
    end

    if type -q nix-shell
        abbr n 'nix'
        abbr ns 'nix-shell --command fish'
    end
end
