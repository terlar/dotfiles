function fish_user_abbreviations
	set -U fish_user_abbreviations

	abbr -a !=sudo
	abbr -a tf='tail -f'
	abbr -a l='ls -a'
	abbr -a l.='ls -d .*'
	abbr -a m=mux

	if type systemctl >/dev/null
		abbr -a j='journalctl --since=yesterday'
		abbr -a je='journalctl --since=yesterday --priority=0..3'
		abbr -a jf='journalctl -f'

		abbr -a sc='sudo systemctl'
		abbr -a scu='systemctl --user'

		abbr -a reboot='sudo systemctl reboot'
		abbr -a poweroff='sudo systemctl poweroff'
		abbr -a suspend='sudo systemctl suspend'
	end

	if type git >/dev/null
		abbr -a g='git'
		abbr -a gs='git status -sb'
		abbr -a gh='git head'

		abbr -a gl='git l'
		abbr -a gll='git ll'
		abbr -a gwc='git wc'

		abbr -a gd='git diff'
		abbr -a gdt='git difftool'
		abbr -a gdc='git diff --cached'
		abbr -a gdw='git diff --color-words'
		abbr -a gds='git diff --stat'

		abbr -a ga='git add'
		abbr -a gc='git commit -v'
		abbr -a gca='git commit -v -a'
		abbr -a gb='git branch'
		abbr -a gco='git checkout'
		abbr -a gf='git fetch'
		abbr -a gfa='git fetch --all'
		abbr -a gm='git merge'
		abbr -a gr='git rebase'
		abbr -a gcp='git cherry-pick'
		abbr -a gpr='git pull-request'
	end

	if type pacman >/dev/null
		abbr -a pac='sudo pacman'
	end

	if type aura >/dev/null
		abbr -a aur='sudo aura'
	end

	if type pygmentize >/dev/null
		abbr -a ccat=pygmentize
	end

	if type bundle >/dev/null
		abbr -a b=bundle
		abbr -a be='bundle exec'
	end

	if type prax >/dev/null
		abbr -a prs='prax restart'
	end

	if type vagrant >/dev/null
		abbr -a v=vagrant
	end

	if type farm >/dev/null
		abbr -a f='farm visit'
		abbr -a fcd='farm cd'
		abbr -a fc='farm console'
		abbr -a fs='farm server'
	end
end
