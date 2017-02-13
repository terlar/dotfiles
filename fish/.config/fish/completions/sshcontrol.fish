function __sshcontrol_seen_any_subcommand_from -a cmd
    __fish_seen_subcommand_from (__sshcontrol_subcommands $cmd | string replace -r '\t.*$' '')
end

function __sshcontrol_subcommands -a cmd
    switch $cmd
        case ''
            echo delete\t'Delete SSH key'
            echo toggle\t'Toggle SSH key'
            echo list\t'List SSH keys'
            echo describe\t'Describe SSH keys'
    end
end

function __sshcontrol_key_completions
    for key in (sshcontrol describe)
        echo $key | read -l state keygrip comment
        echo $keygrip\t"$comment $state"
    end
end

complete -c sshcontrol -n 'not __sshcontrol_seen_any_subcommand_from ""' -f -a '(__sshcontrol_subcommands "")'

complete -c sshcontrol -n '__fish_seen_subcommand_from delete toggle' -f -a '(__sshcontrol_key_completions)'
