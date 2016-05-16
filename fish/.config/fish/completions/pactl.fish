# fish completion for pactl
function __pactl_needs_command
    set cmd (commandline -opc)
    if [ (count $cmd) -eq 1 -a $cmd[1] = 'pactl' ]
        return 0
    end
    return 1
end

function __pactl_using_command
    set cmd (commandline -opc)
    if [ (count $cmd) -gt 1 ]
        if [ $argv[1] = $cmd[2] ]
            return 0
        end
    end
    return 1
end

function __pactl_sinks
    pactl list short sinks | string replace -r '^(\d+)\s+([^\s]+).*' '$1'\t'Sink: $2'
end

function __pactl_sources
    pactl list short sources | string replace -r '^(\d+)\s+([^\s]+).*' '$1'\t'Source: $2'
end

function __pactl_sink-inputs
    pactl list short sink-inputs | string replace -r '^(\d+).*' '$1'
end

function __pactl_source-outputs
    pactl list short source-outputs | string replace -r '^(\d+).*' '$1'
end

# List
complete -f -c pactl -n '__pactl_needs_command' -a list
complete -f -c pactl -n '__pactl_using_command list' -a short
complete -f -c pactl -n '__pactl_using_command list' -a 'modules sinks sources sink-inputs source-outputs clients samples cards' -d 'Type'

# Move
complete -f -c pactl -n '__pactl_needs_command' -a 'move-sink-input move-source-output'
complete -f -c pactl -n '__pactl_using_command move-sink-input' -a '(__pactl_sinks)'
complete -f -c pactl -n '__pactl_using_command move-source-output' -a '(__pactl_sinks)'
complete -f -c pactl -n '__pactl_using_command move-sink-input' -a '(__pactl_sink-inputs)' -d 'Sink input'
complete -f -c pactl -n '__pactl_using_command move-source-output' -a '(__pactl_source-outputs)' -d 'Source output'

# Default
complete -f -c pactl -n '__pactl_needs_command' -a 'set-default-sink set-default-source'
complete -f -c pactl -n '__pactl_using_command set-default-sink' -a '(__pactl_sinks)'
complete -f -c pactl -n '__pactl_using_command set-default-source' -a '(__pactl_sources)'

# Volume
complete -f -c pactl -n '__pactl_needs_command' -a 'set-sink-volume set-source-volume'
complete -f -c pactl -n '__pactl_using_command set-sink-volume' -a '(__pactl_sinks)'
complete -f -c pactl -n '__pactl_using_command set-source-volume' -a '(__pactl_sources)'
