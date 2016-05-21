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
update: pull install

.PHONY: pull
pull:
	git pull
