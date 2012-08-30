ZSH=$HOME/.zsh

fpath=($ZSH/functions $ZSH/completions $fpath)
autoload -U $ZSH/functions/*(:t)
autoload -U $ZSH/completions/*(:t)
autoload colors; colors;

for config ($ZSH/*.zsh) source $config

for plugin ($ZSH/plugins/*); do
  if [ -d $plugin ]; then
    plugin=$plugin/${plugin##*/}.zsh
  fi

  if [ -f $plugin ]; then
    source $plugin
  fi
done
