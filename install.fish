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

function dotfiles_remove_file
  echo $argv | read -l target_file

  set_color red --bold
  echo -n '==> '
  set_color normal
  echo Remove $target_file

  rm -r $target_file
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

    if test -e $target_file -o -L $target_file
      dotfiles_read_choice $target_file
      if set -qg choice_overwrite
        dotfiles_remove_file $target_file
      else if set -qg choice_backup
        dotfiles_backup_file $target_file
      else
        continue
      end
    end

    dotfiles_link_file $target_file $source_file
  end
end

function dotfiles_install_vim
  git clone git://github.com/terlar/vimfiles.git $HOME/.vimfiles
  ln -s $HOME/.vimfiles/vim $HOME/.vim
  ln -s $HOME/.vimfiles/vimrc $HOME/.vimrc
end

set -l current_file (basename (status -f))
set -l files (ls | cat | grep -v $current_file | grep -v README.md)
set -eg choice_all

echo 'Installing dotfiles...'

dotfiles_install $HOME/. $PWD/ $files
dotfiles_install_vim

set_color blue --bold
echo -n '==> '
set_color normal
echo 'DONE!'

functions -e dotfiles_read_choice
functions -e dotfiles_read_choice_prompt
functions -e dotfiles_install
functions -e dotfiles_remove_file
functions -e dotfiles_backup_file
functions -e dotfiles_link_file
