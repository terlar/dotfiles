# Alias
function zr   ; zeus rake $argv; end
function zc   ; zeus console $argv; end
function zs   ; zeus server $argv; end
function zg   ; zeus generate $argv; end
function zdb  ; zeus dbconsole $argv; end
function zt   ; zeus test $argv; end

functions -c rspec __pre_zeus_rspec
function rspec ; zeus_rspec $argv; end