function __fish_sandbox_needs_command
    set cmd (commandline -opc)
    if [ (count $cmd) -eq 1 -a $cmd[1] = 'sandbox' ]
        return 0
    end
    return 1
end

function __fish_sandbox_using_command
    set cmd (commandline -opc)
    if [ (count $cmd) -gt 1 ]
        if [ $argv[1] = $cmd[2] ]
            return 0
        end
    end
    return 1
end

function __fish_sandbox_commands
    sandbox help \
        | grep '^  ' \
        | string replace -r '^\s+([^\s]+)\s+(.+)$' '$1'\t'$2'
end

function __fish_sandbox_overrides
    sandbox dev | grep ' -' | string replace ' - ' ''
end

complete -f -c sandbox -n '__fish_sandbox_needs_command' -s 'v' -d 'Verbose output'
complete -f -c sandbox -n '__fish_sandbox_needs_command' -a '(__fish_sandbox_commands)'
complete -f -c sandbox -n '__fish_sandbox_using_command dev' -a '(__fish_sandbox_overrides)' -d 'Service'
