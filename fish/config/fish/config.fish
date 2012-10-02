set fish_greeting

setenv TERM xterm-256color
setenv EDITOR vim
setenv VISUAL $EDITOR


# Paths
set -U FISH $HOME/.config/fish

set PATH $HOME/.local/bin /usr/local/bin $PATH
# Ry
set PATH $HOME/.local/lib/ry/current/bin $PATH

set -U NODE_PATH /usr/local/lib/node_modules
set -U REMOTE_GEM_CACHE_PATH $HOME/.remote-gem-cache


# Plugins
for plugin in $FISH/plugins/*
  if test -d $plugin
    set -xg fish_function_path $plugin/functions $fish_function_path
    set -xg fish_complete_path $plugin/completions $fish_complete_path
    set plugin $plugin/(basename $plugin).fish
  end

  if test -f $plugin
    . $plugin
  end
end


# Alias
function l      ; ls -la ; end
function l.     ; ls -d .* ; end
function ll     ; ls -l ; end

function mkdir  ; mkdir -p $argv ; end
function mkcd   ; mkdir $argv; and cd $argv ; end

function !      ; sudo $argv ; end
function rc     ; rails console ; end
