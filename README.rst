================================================================================
Configuration files
================================================================================

This repository contains my configuration files. Each directory contains
a group of configuration files for either a single application or
a purpose. The destination of these configuration files should be ``$HOME``.

The configuration can easily be linked with `GNU stow`_.

Installation
================================================================================

Install everything
--------------------------------------------------------------------------------

.. code:: sh

        git clone git://github.com/terlar/dotfiles.git ~/.dotfiles
        cd ~/.dotfiles
        make install

Install group(s)
--------------------------------------------------------------------------------

A ``group`` is corresponding to a directory in the project root.

.. code:: sh

        git clone git://github.com/terlar/dotfiles.git ~/.dotfiles
        cd ~/.dotfiles
        make install PKGS=group
        make install PKGS=group1,group2

Update
================================================================================

Pull the latest dotfiles and install.

.. code:: sh

        cd ~/.dotfiles
        make update

Uninstall
================================================================================

Unlinks the dotfiles.

.. code:: sh

        cd ~/.dotfiles
        make uninstall
        rm -rf ~/.dotfiles

.. _`GNU stow`: https://www.gnu.org/software/stow/
