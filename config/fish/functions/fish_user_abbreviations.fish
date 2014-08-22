function fish_user_abbreviations
	set -U fish_user_abbreviations

	abbr ! sudo
	abbr tf 'tail -f'
	abbr l 'ls -a'
	abbr l. 'ls -d .*'

	if type systemctl >/dev/null
		abbr j 'journalctl --since=yesterday'
		abbr je 'journalctl --since=yesterday --priority=0..3'
		abbr jf 'journalctl -f'

		abbr sc 'sudo systemctl'
		abbr scu 'systemctl --user'

		abbr reboot 'sudo systemctl reboot'
		abbr poweroff 'sudo systemctl poweroff'
		abbr suspend 'sudo systemctl suspend'

		abbr stx 'systemctl --user start desktop.target'
		abbr klx 'systemctl --user start console.target'
		abbr m 'systemctl --user start mopidy'
	end

	if type git >/dev/null
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
		abbr gc 'git commit -v'
		abbr gca 'git commit -v -a'
		abbr gb 'git branch'
		abbr gco 'git checkout'
		abbr gf 'git fetch'
		abbr gfa 'git fetch --all'
		abbr gm 'git merge'
		abbr gr 'git rebase'
		abbr gcp 'git cherry-pick'
		abbr gpr 'git pull-request'
	end

	if type pacman >/dev/null
		abbr pac 'sudo pacman'
	end

	if type aura >/dev/null
		abbr aur 'sudo aura'
	end

	if type pygmentize >/dev/null
		abbr ccat pygmentize
	end

	if type bundle >/dev/null
		abbr b bundle
		abbr be 'bundle exec'
	end

	if type prax >/dev/null
		abbr prs 'prax restart'
	end

	if type vagrant >/dev/null
		abbr v vagrant
	end

	if type farm >/dev/null
		abbr f 'farm visit'
		abbr fcd 'farm cd'
		abbr fc 'farm console'
		abbr fs 'farm server'
	end
end
