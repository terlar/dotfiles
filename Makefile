.DEFAULT_GOAL:=install

.PHONY: install
install:
	stow -t ~ $(sort $(dir $(wildcard */)))

.PHONY: uninstall
uninstall:
	stow -Dt ~ $(sort $(dir $(wildcard */)))

.PHONY: update
update: pull install

.PHONY: pull
pull:
	git pull
