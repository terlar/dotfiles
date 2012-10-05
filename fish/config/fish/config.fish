set fish_greeting

setenv TERM xterm-256color
setenv EDITOR vim


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
function !      ; sudo $argv ; end
function rc     ; rails console ; end

# ls
function l      ; ls -la ; end
function l.     ; ls -d .* ; end
function ll     ; ls -l ; end

# mkdir
function mkdir  ; mkdir -p $argv ; end
function mkcd   ; mkdir $argv; and cd $argv ; end

# git
function g    ; git $argv ; end
function gs   ; git status $argv ; end

function gd   ; git diff $argv ; end
function gdv  ; git diff -w $argv | view - ; end
function gwc  ; git wc $argv ; end

function ga   ; git add $argv ; end
function gc   ; git commit -v $argv ; end
function gca  ; git commit -v -a $argv ; end

function gb   ; git branch $argv ; end
function gco  ; git checkout $argv ; end

function gf   ; git fetch $argv ; end
function gm   ; git merge $argv ; end
function gr   ; git rebase $argv ; end
