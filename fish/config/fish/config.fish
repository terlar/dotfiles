if status --is-login
  set fish_greeting

  set -x TERM xterm-256color
  set -x EDITOR vim
  set -x LC_ALL en_US.UTF-8

  # Less colors
  set -x LESS_TERMCAP_mb \e'[01;31m'       # begin blinking
  set -x LESS_TERMCAP_md \e'[01;38;5;74m'  # begin bold
  set -x LESS_TERMCAP_me \e'[0m'           # end mode
  set -x LESS_TERMCAP_so \e'[38;5;246m'    # begin standout-mode - info box
  set -x LESS_TERMCAP_se \e'[0m'           # end standout-mode
  set -x LESS_TERMCAP_us \e'[04;38;5;146m' # begin underline
  set -x LESS_TERMCAP_ue \e'[0m'           # end underline

  # Paths
  set PATH $HOME/.local/bin /usr/local/bin $PATH
  set -x NODE_PATH /usr/local/lib/node_modules
end


# Plugins
set -l plugins_path (dirname (status -f))/plugins
# Bundler
. $plugins_path/bundler/bundler.fish

# Farm
. /usr/local/share/fish-farm/farm.fish
# Fry
. /usr/local/share/fry/fry.fish


# Alias
function !      ; sudo $argv ; end
function tf     ; tail -f $argv; end

# ls
function l      ; ls -la ; end
function l.     ; ls -d .* ; end

# mkdir
function mkdir  ; command mkdir -p $argv ; end
function mkcd   ; mkdir $argv; and cd $argv ; end

# git
function g    ; git $argv ; end
function gs   ; git status $argv ; end

function gl   ; git l $argv ; end
function gls  ; git ls $argv ; end
function gwc  ; git wc $argv ; end

function gd   ; git diff $argv ; end
function gdw  ; git diff --color-words $argv ; end
function gds  ; git diff --stat $argv ; end
function gdv  ; git diff -w $argv | view - ; end

function ga   ; git add $argv ; end
function gc   ; git commit -v $argv ; end
function gca  ; git commit -v -a $argv ; end

function gb   ; git branch $argv ; end
function gco  ; git checkout $argv ; end

function gf   ; git fetch $argv ; end
function gm   ; git merge $argv ; end
function gr   ; git rebase $argv ; end

# ruby
function rc ; rails console $argv ; end
function rg ; rails generate $argv ; end
function be ; bundle exec $argv; end
function s  ; spring $argv; end
function sb
  which spring >/dev/null; or return

  for i in rspec rake rails
    spring binstub $i
  end
end

function b
  bundle $argv
  and begin
    test (count $argv) = 0; and sb
    return 0
  end
end

# Tools
function kc ; kviberg-config $argv ; end
