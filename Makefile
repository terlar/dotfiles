.DEFAULT_GOAL := help

DESTDIR  ?= $$HOME
PACKAGES ?= $(sort $(dir $(wildcard */)))

TS := .ts
XMONAD_FILES := \
	xmonad/.xmonad/xmonad.hs \
	$(shell find xmonad/.xmonad/lib -type f -name '*.hs')

comma:= ,
empty:=
space:= $(empty) $(empty)

$(TS)/.xmonad: $(XMONAD_FILES)
	xmonad --recompile
	xmonad --restart
	@mkdir -p $(@D)
	@touch $@

.PHONY: install
install: ## Install PACKAGES (all or use PACKAGES=package)
	stow -t $(DESTDIR) $(subst $(comma),$(space),$(PACKAGES))

.PHONY: uninstall
uninstall: ## Uninstall PACKAGES (all or use PACKAGES=package)
	stow -Dt $(DESTDIR) $(subst $(comma),$(space),$(PACKAGES))

.PHONY: update
update: ## Update and install
update: pull update-submodules install

.PHONY: pull
pull: ## Pull latest changes
	git pull

.PHONY: update-submodules
update-submodules: ## Update all submodules
	git submodule sync
	git submodule update --init --recursive --remote

.PHONY: xmonad
xmonad: ## Compile and restart xmonad
xmonad: $(TS)/.xmonad

.PHONY: gems
gems: ## Install system ruby gems
	gem install bundler
	bundle install --system --gemfile ruby/.Gemfile

.PHONY: help
help: ## Describe tasks
	$(info Tasks:)
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST)
