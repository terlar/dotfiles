.DEFAULT_GOAL := install

DESTDIR  ?= $$HOME
PACKAGES ?= $(sort $(dir $(wildcard */)))

.PHONY: install
install:
	stow -t $(DESTDIR) $(PACKAGES)

.PHONY: uninstall
uninstall:
	stow -Dt $(DESTDIR) $(PACKAGES)

.PHONY: update
update: pull update-submodules install

.PHONY: pull
pull:
	git pull

.PHONY: update-submodules
update-submodules:
	git submodule sync
	git submodule update --init --recursive --remote
