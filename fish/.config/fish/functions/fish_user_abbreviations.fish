function fish_user_abbreviations
	set -U fish_user_abbreviations

	abbr tf 'tail -f'
	abbr l 'ls -a'
	abbr l. 'ls -d .*'
	abbr m mux
	abbr e $EDITOR
	abbr week "date '+%V'"

	if type -fq systemctl
		abbr j 'journalctl --since=yesterday'
		abbr je 'journalctl --since=yesterday --priority=0..3'
		abbr jf 'journalctl -f'
		abbr ju 'journalctl -u'
		abbr juu 'journalctl --user-unit'

		abbr sc 'sudo systemctl'
		abbr scu 'systemctl --user'
		abbr scs 'systemctl status'
		abbr scl 'systemctl list-units'

		abbr reboot 'sudo systemctl reboot'
		abbr poweroff 'sudo systemctl poweroff'
		abbr suspend 'sudo systemctl suspend'
	end

	if type -fq git
		abbr g 'git'
		abbr gs 'git status -sb'
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
		abbr gb 'git branch'
		abbr gco 'git checkout'
		abbr gf 'git fetch'
		abbr gfa 'git fetch --all'
		abbr gm 'git merge'
		abbr gr 'git rebase'
		abbr gcp 'git cherry-pick'
		abbr gpr 'git-pull-request'
	end

	if type -fq pacman
		abbr pac 'sudo pacman'
	end

	if type -fq aura
		abbr aur 'sudo aura'
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
		abbr f 'farm visit'
		abbr fcd 'farm cd'
	end
end
