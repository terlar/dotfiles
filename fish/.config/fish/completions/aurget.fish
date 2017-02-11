# aurget - a simple pacman-like interface to the AUR
# See: https://github.com/pbrisbin/aurget

function __aurget_current_token_is_option
    string match -qr -- '^--?[A-Za-z0-9-]*$' (commandline -pt)
end

function __aurget_search_aur_packages -a term
    test (string length $term) -gt 2
    or return

    type -q curl
    or return

    type -q jq
    or return

    curl -sS "https://aur.archlinux.org/rpc/?v=5&type=search&arg=$term" | jq -r '.results[] | .Name + "\t" + .Description'
end

complete -c aurget -n '__fish_seen_subcommand_from -S -Sd -Sb -Sy -Sp -Si' -f -a '(__aurget_search_aur_packages (commandline -pt))'

complete -c aurget -n '__aurget_current_token_is_option' -f -a '-S' -d 'Process <package> using your default sync_mode'
complete -c aurget -n '__aurget_current_token_is_option' -f -a '-Sd' -d 'Download <package>'
complete -c aurget -n '__aurget_current_token_is_option' -f -a '-Sb' -d 'Download and build <package>'
complete -c aurget -n '__aurget_current_token_is_option' -f -a '-Sy' -d 'Download, build and install <package>'

complete -c aurget -n '__aurget_current_token_is_option' -f -a '-Su' -d 'Process available upgrades using your default sync_mode'

complete -c aurget -n '__aurget_current_token_is_option' -f -a '-Sdu' -d 'Download available upgrades'
complete -c aurget -n '__aurget_current_token_is_option' -f -a '-Sbu' -d 'Download and build available upgrades'
complete -c aurget -n '__aurget_current_token_is_option' -f -a '-Syu' -d 'Download, build, and install available upgrades'

complete -c aurget -n '__aurget_current_token_is_option' -f -a '-Ss' -d 'Search AUR for <term>'
complete -c aurget -n '__aurget_current_token_is_option' -f -a '-Ssq' -d 'Search aur for <term>, print only package names'
complete -c aurget -n '__aurget_current_token_is_option' -f -a '-Sp' -d 'Print the PKGBUILD for <package>'
complete -c aurget -n '__aurget_current_token_is_option' -f -a '-Si' -d 'Print extended info for <package>'

complete -c aurget -l sort -x -a 'name votes' -d 'Sort search output by name (ascending) or votes (descending)'
complete -c aurget -l builddir -d 'Build in <directory>'
complete -c aurget -n '__fish_seen_subcommand_from -Su' -l devel -f -d 'Add all development packages'
complete -c aurget -l deps -f -d 'Resolve dependencies'
complete -c aurget -l nodeps -f -d "Don't resolve dependencies"
complete -c aurget -l edit -f -d 'Prompt to edit all pkgbuilds'
complete -c aurget -l noedit -f -d "Don't prompt to edit any pkgbuilds"
complete -c aurget -l discard -f -d 'Discard source files after building'
complete -c aurget -l nodiscard -f -d "Don't discard source files after building"
complete -c aurget -l nocolor -f -d 'Disable coloring'
complete -c aurget -l noconfirm -f -d 'Auto-answer all prompts'
complete -c aurget -l ignore -f -d 'Add additional packages to be ignored'
complete -c aurget -l config -d 'Source <file> for user configuration'
complete -c aurget -s h -l help -f -d 'Display the help'
