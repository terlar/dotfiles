.DEFAULT_GOAL:=install

PACKAGES=$(sort $(dir $(wildcard */)))

.PHONY: install
install:
	stow -t ~ $(PACKAGES)

.PHONY: uninstall
uninstall:
	stow -Dt ~ $(PACKAGES)

.PHONY: update
update: pull install

.PHONY: pull
pull:
	git pull
