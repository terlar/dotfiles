## Installation

During the installation you will get an interactive menu for any
conflicting file. You can choose to either keep, replace or backup the
old files.

```sh
git clone git://github.com/terlar/dotfiles.git ~/.dotfiles
fish ~/.dotfiles/install.fish
```

## Update

Pull the latest dotfiles.

```sh
cd ~/.dotfiles
git pull
```

## Uninstall

Unlinks the dotfiles and restores eventual backups.

```sh
fish ~/.dotfiles/uninstall.fish
rm -rf ~/.dotfiles
```
