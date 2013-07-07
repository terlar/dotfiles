function dotfiles_read_choice
  set -l target $argv[1]

  if set -qg choice_all
    return
  else
    set -eg choice_overwrite
    set -eg choice_backup
    set -eg choice_skip
  end

  while true
    echo "File already exists: '$target', what do you want to do?"
    read -l -p dotfiles_read_choice_prompt choice

    switch $choice
      case O
        set -g choice_all 1
        set -g choice_overwrite 1
        return
      case o
        set -g choice_overwrite 1
        return
      case B
        set -g choice_all 1
        set -g choice_backup 1
        return
      case b
        set -g choice_backup 1
        return
      case S
        set -g choice_all 1
        set -g choice_skip 1
        return
      case '' s
        set -g choice_skip 1
        return
    end
  end
end

function dotfiles_read_choice_prompt
  echo '- [s]kip, [S]kip all'
  echo '- [o]verwrite, [O]verwrite all'
  echo '- [b]ackup, [B]ackup all'
  echo '> '
end

function dotfiles_read_confirm
  while true
    read -l -p dotfiles_read_confirm_prompt confirm

    switch $confirm
      case Y y
        return 0
      case '' N n
        return 1
    end
  end
end

function dotfiles_read_confirm_prompt
  echo 'Do you want to continue? [Y/n] '
end

function dotfiles_remove_file
  echo $argv | read -l target_file

  set_color red --bold
  echo -n '==> '
  set_color normal
  echo Remove $target_file

  rm -rf $target_file
end

function dotfiles_backup_file
  echo $argv | read -l target_file

  set_color blue --bold
  echo -n '==> '
  set_color normal
  echo Backup $target_file

  mv $target_file $target_file.backup
end

function dotfiles_link_file
  echo $argv | read -l target_file source_file

  set_color green --bold
  echo -n '==> '
  set_color normal
  echo Link $target_file

  ln -s $source_file $target_file
end

function dotfiles_prepare_linking
  if test -e $argv -o -L $argv
    dotfiles_read_choice $argv
    if set -qg choice_overwrite
      dotfiles_remove_file $argv
    else if set -qg choice_backup
      dotfiles_backup_file $argv
    else
      return 1
    end
  end

  return 0
end

function dotfiles_install
  echo $argv[1..2] | read -l target_prefix source_prefix
  set -e argv[1..2]
  set -l files $argv

  for file in $argv
    set -l source_file $source_prefix$file
    set -l target_file $target_prefix$file
    set -l target_link (readlink $target_file)

    if test "$target_link" = $source_file
      continue
    end

    if test -d $source_file
      if not test -d $target_file
        dotfiles_link_file $target_file $source_file
      else
        dotfiles_install $target_file/ $source_file/ (ls $source_file)
      end

      continue
    end

    if dotfiles_prepare_linking $target_file
      dotfiles_link_file $target_file $source_file
    end
  end
end

function dotfiles_install_vim
  if dotfiles_prepare_linking $HOME/.vim
    git clone git://github.com/terlar/vimfiles.git $HOME/.vim
  else
    return
  end

  if dotfiles_prepare_linking $HOME/.vimrc
    ln -s $HOME/.vim/vimrc $HOME/.vimrc
  end
end

set -l current_file (basename (status -f))
set -l files (ls | cat | grep -vE "($current_file|README.md)")
set -eg choice_all

echo 'Installing dotfiles...'
if dotfiles_read_confirm
  dotfiles_install $HOME/. $PWD/ $files

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

echo 'Installing vimfiles...'
if dotfiles_read_confirm
  dotfiles_install_vim

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

functions -e dotfiles_install
functions -e dotfiles_install_vim
functions -e dotfiles_read_choice
functions -e dotfiles_read_choice_prompt
functions -e dotfiles_read_confirm
functions -e dotfiles_read_confirm_prompt
functions -e dotfiles_prepare_linking
functions -e dotfiles_remove_file
functions -e dotfiles_backup_file
functions -e dotfiles_link_file
