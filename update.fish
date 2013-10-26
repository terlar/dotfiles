set -l dotfiles_dir (dirname (status -f))

cd $dotfiles_dir
echo 'Updating dotfiles...'
git pull >/dev/null
set_color blue --bold
echo -n '==> '
set_color normal
echo 'DONE!'

if test -d $HOME/.vim
  cd $HOME/.vim
  git pull >/dev/null
  echo 'Updating vimfiles...'
  set_color blue --bold
  echo -n '==> '
  set_color normal
  echo 'DONE!'
end
