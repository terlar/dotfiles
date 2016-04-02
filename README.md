## Installation

### Install everything

```sh
git clone git://github.com/terlar/dotfiles.git ~/.dotfiles
make install
```

### Install components

```sh
git clone git://github.com/terlar/dotfiles.git ~/.dotfiles
stow component
```

## Update

Pull the latest dotfiles and install.

```sh
make update
```

## Uninstall

Unlinks the dotfiles.

```sh
make uninstall
rm -rf ~/.dotfiles
```
