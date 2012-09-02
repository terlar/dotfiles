## Installation

During the installation you will get an interactive menu for any
conflicting file. You can choose to either keep, replace or backup the
old files.

```
git clone git://github.com/terlar/dotfiles.git ~/.dotfiles
cd ~/.dotfiles
rake install
```

## Update

Pulls the latest dotfiles and updates the submodules.

```
cd ~/.dotfiles
rake update
```

## Uninstall

Unlinks the dotfiles and restores eventual backups.

```
cd ~/.dotfiles
rake uninstall
rm -rf ~/.dotfiles
```
