# Initialize
set -l zeus_path (dirname (status -f))

if not contains $zeus_path/functions $fish_function_path
  set fish_function_path $zeus_path/functions $fish_function_path
end


# Alias
function zc   ; zeus console $argv; end
function zr   ; zeus rake $argv; end
function zs   ; zeus server $argv; end
function zg   ; zeus generate $argv; end
function zdb  ; zeus dbconsole $argv; end
function zt   ; zeus test $argv; end

function rc   ; zeus_rc $argc; end

function rake ; zeus_rake $argv; end
function rspec ; zeus_rspec $argv; end
