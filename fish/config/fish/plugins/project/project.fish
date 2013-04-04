# Paths
set -q fry_rubies ; or set -U PROJECTS $HOME/Code
set CDPATH $CDPATH $PROJECTS

# Alias
function p ; project_switch $argv ; end
