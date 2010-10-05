# Path for MacPorts
export PATH=/opt/local/bin:/opt/local/sbin:$PATH

# mkdir, cd into it
mkcd () {
    mkdir -p "$*"
    cd "$*"
}
