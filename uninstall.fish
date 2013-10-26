function __dotfiles_read_confirm
  while true
    read -l -p __dotfiles_read_confirm_prompt confirm

    switch $confirm
      case '' Y y
        return 0
      case N n
        return 1
    end
  end
end

function __dotfiles_read_confirm_prompt
  echo 'Do you want to continue? [Y/n] '
end

function __dotfiles_restore_file
  echo $argv | read -l target_file backup_file

  set_color green --bold
  echo -n '==> '
  set_color normal
  echo Restore $target_file

  mv $backup_file $target_file
end

function __dotfiles_unlink_file
  echo $argv | read -l target_file

  set_color red --bold
  echo -n '==> '
  set_color normal
  echo Unlink $target_file

  rm $target_file
end

function __dotfiles_remove_dir
  echo $argv | read -l target_file

  set_color red --bold
  echo -n '==> '
  set_color normal
  echo Removing $target_file

  rm -rf $target_file
end

function dotfiles_uninstall
  echo $argv[1..2] | read -l target_prefix source_prefix
  set -e argv[1..2]

  for file in $argv
    set -l source_file $source_prefix$file
    set -l target_file $target_prefix$file
    set -l target_link (readlink $target_file)

    if not test "$target_link" = $source_file
      if test -d $source_file
        dotfiles_uninstall $target_file/ $source_file/ (ls $source_file)
      end

      continue
    end

    set -l backup_file $target_file.backup

    __dotfiles_unlink_file $target_file

    if test -e $backup_file
      __dotfiles_restore_file $target_file $backup_file
    end
  end
end

function dotfiles_uninstall_vim
  set -l vim_dir $HOME/.vim
  set -l vim_backup $HOME/.vim.backup
  set -l vimrc_file $HOME/.vimrc
  set -l vimrc_backup $HOME/.vimrc.backup

  if test -e $vim_dir
    __dotfiles_remove_dir $vim_dir
  end
  if test -e $vim_backup
    __dotfiles_restore_file $vim_dir $vim_backup
  end

  if test -f $vimrc_file
    __dotfiles_unlink_file $vimrc_file
  end
  if test -f $vimrc_backup
    __dotfiles_restore_file $vimrc_file $vimrc_backup
  end
end

set -l dotfiles_dir (dirname (status -f))
set -l files (ls $dotfiles_dir | cat | grep -vE "(install.fish|update.fish|uninstall.fish|README.md)")

echo 'Uninstalling dotfiles...'
if __dotfiles_read_confirm
  dotfiles_uninstall $HOME/. $dotfiles_dir/ $files

  set_color blue --bold
  echo -n '==> '
  set_color normal
  echo 'DONE!'
else
  set_color red --bold
  echo -n '==> '
  set_color normal
  echo 'SKIPPED!'
end

echo 'Uninstalling vimfiles...'
if __dotfiles_read_confirm
  dotfiles_uninstall_vim

  set_color blue --bold
  echo -n '==> '
  set_color normal
  echo 'DONE!'
else
  set_color red --bold
  echo -n '==> '
  set_color normal
  echo 'SKIPPED!'
end

functions -e dotfiles_uninstall
functions -e dotfiles_uninstall_vim
functions -e __dotfiles_read_confirm
functions -e __dotfiles_read_confirm_prompt
functions -e __dotfiles_restore_file
functions -e __dotfiles_unlink_file
functions -e __dotfiles_remove_dir
