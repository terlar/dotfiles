set fish_greeting

set -x TERM xterm-256color
set -x EDITOR vim


# Paths
set -g CDPATH .

set -l path_list /usr/local/bin $HOME/.local/bin
# Ry
set path_list $path_list $HOME/.local/lib/ry/current/bin

for p in $path_list
  if test -d $p
    set PATH $p $PATH
  end
end

set -g NODE_PATH /usr/local/lib/node_modules
set -g REMOTE_GEM_CACHE_PATH $HOME/.remote-gem-cache


# Plugins
for plugin in $HOME/.config/fish/plugins/*
  if test -d $plugin
    if not contains $plugin/functions $fish_function_path
      set fish_function_path $plugin/functions $fish_function_path
    end
    if not contains $plugin/completions $fish_complete_path
      set fish_complete_path $plugin/completions $fish_complete_path
    end

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
