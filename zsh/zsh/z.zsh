# Z
if [ -f $HOME/.local/z.sh ]; then
  source $HOME/.local/z.sh
  _z-init () {
    _z --add "$(pwd -P)"
  }
  precmd_functions+=(_z-init)
fi
