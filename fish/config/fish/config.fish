set fish_greeting


# Paths
set -U FISH $HOME/.config/fish
set -U PROJECTS $HOME/Code
set -U CDPATH . $PROJECTS

set -x PATH $HOME/.local/bin /usr/local/bin $PATH
# Ry
set -U RY_PREFIX $HOME/.local
set -x PATH $RY_PREFIX/lib/ry/current/bin $PATH

set -U NODE_PATH /usr/local/lib/node_modules
set -U REMOTE_GEM_CACHE_PATH $HOME/.remote-gem-cache


# Plugins
for plugin in $FISH/plugins/*
  if test -d $plugin
    set -g fish_function_path $plugin/functions $fish_function_path
    set -g fish_complete_path $plugin/completions $fish_complete_path
    set plugin $plugin/(basename $plugin).fish
  end

  if test -f $plugin
    . $plugin
  end
end


# Alias
function l. ; l -d .* ; end
function ll ; l -l ; end
function mkdir ; mkdir -p $argv ; end
function mkcd ; mkdir $argv; and cd $argv ; end

function rc ; rails c ; end
