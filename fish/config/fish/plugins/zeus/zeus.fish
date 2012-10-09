# Alias
function zc   ; zeus console $argv; end
function zr   ; zeus rake $argv; end
function zs   ; zeus server $argv; end
function zg   ; zeus generate $argv; end
function zdb  ; zeus dbconsole $argv; end
function zt   ; zeus test $argv; end

function rc   ; zeus_rc $argc; end

functions -c rake __pre_zeus_rake
function rake ; zeus_rake $argv; end

functions -c rspec __pre_zeus_rspec
function rspec ; zeus_rspec $argv; end
