# Alias
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


# Prompt
function __misc_prompt_git --on-event misc_prompt
  set -g misc_prompt (git_prompt) $misc_prompt
end
