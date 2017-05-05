.DEFAULT_GOAL := help

DESTDIR  ?= $$HOME
PKGS ?= $(sort $(dir $(wildcard */)))

TS := .ts
XMONAD_FILES := \
	xmonad/.xmonad/xmonad.hs \
	$(shell find xmonad/.xmonad/lib -type f -name '*.hs')

REAL_DIRS := $(addprefix $(DESTDIR)/,\
	.config/env\
	.gnupg .ssh\
	.mutt .mutt/temp .offlineimap\
	.config/qutebrowser\
	.ncmpcpp .config/mopidy\
	.xmonad/lib\
	.config/termite\
	.config/fish/conf.d .config/fish/completions .config/fish/functions\
	.sbt/0.13/plugins)

comma:= ,
empty:=
space:= $(empty) $(empty)

$(TS)/.xmonad: $(XMONAD_FILES)
	xmonad --recompile
	xmonad --restart
	@mkdir -p $(@D)
	@touch $@

$(REAL_DIRS):
	@mkdir -p $@

.PHONY: install
install: ## Install PKGS (all or use PKGS=package)
install: dirs
	$(info ===> Install files)
	stow -t $(DESTDIR) $(subst $(comma),$(space),$(PKGS))

.PHONY: uninstall
uninstall: ## Uninstall PKGS (all or use PKGS=package)
	$(info ===> Uninstall files)
	stow -Dt $(DESTDIR) $(subst $(comma),$(space),$(PKGS))

.PHONY: update
update: ## Update and install
update: pull update-submodules install

.PHONY: pull
pull: ## Pull latest changes
	$(info ===> Fetch changes)
	git pull

.PHONY: config
config: ## Interactive configuration
	$(info ==> Start interactive configuration)
	@./interactive-config

.PHONY: dconf
dconf:
	./apply-dconf $(DESTDIR)/.local/share/dconf

.PHONY: dirs
dirs: ## Make directories to prevent symlinking them
dirs: $(REAL_DIRS)
	$(info ===> Make directories)

.PHONY: update-submodules
update-submodules: ## Update all submodules
	$(info ===> Update submodules)
	git submodule sync
	git submodule update --init --recursive --remote

.PHONY: xmonad
xmonad: ## Compile and restart xmonad
xmonad: $(TS)/.xmonad

.PHONY: systemd-reload
systemd-reload: ## Reload systemd
	systemctl --user daemon-reload

.PHONY: gems
gems: ## Install system ruby gems
	$(info ===> Install gems)
	gem install bundler
	bundle install --system --gemfile ruby/.Gemfile

.PHONY: help
help: ## Describe tasks
	$(info Tasks:)
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST)
