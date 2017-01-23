function __fish_sre_needs_command
    set cmd (commandline -opc)
    if [ (count $cmd) -eq 1 -a $cmd[1] = 'sre' ]
        return 0
    end
    return 1
end

function __fish_sre_using_command
    set cmd (commandline -opc)
    if [ (count $cmd) -gt 1 ]
        if [ $argv[1] = $cmd[2] ]
            return 0
        end
    end
    return 1
end

complete -f -c sre -n '__fish_sre_needs_command' -a 'apollo-chainsaw' -d 'Run an apollo update on all production things'
complete -f -c sre -n '__fish_sre_needs_command' -a 'aws' -d 'Manage the roles, policies, groups, IAM users, etc in our AWS account family'
complete -f -c sre -n '__fish_sre_needs_command' -a 'dns' -d 'Print all CloudFlare DNS entries'
complete -f -c sre -n '__fish_sre_needs_command' -a 'help' -d 'Print all CloudFlare DNS entries'
complete -f -c sre -n '__fish_sre_needs_command' -a 'prod' -d 'The prod subcommand is a utility for interacting with the production enviornment'

complete -f -c sre -n '__fish_sre_using_command aws' -a 'accesskeys' -d 'Generate access keys for the IAM_USER_NAME in the main account'
complete -f -c sre -n '__fish_sre_using_command aws' -a 'addgroup' -d 'Add IAM_USER_NAME to GROUP in the main account'
complete -f -c sre -n '__fish_sre_using_command aws' -a 'addgroups' -d 'Add IAM_USER_NAME to provided GROUPs in the main account'
complete -f -c sre -n '__fish_sre_using_command aws' -a 'create' -d 'Create an new IAM user account named IAM_USER_NAME'
complete -f -c sre -n '__fish_sre_using_command aws' -a 'createmfa' -d 'Create a new virtual MFA device named IAM_USER_NAME'
complete -f -c sre -n '__fish_sre_using_command aws' -a 'delete' -d 'Delete the IAM user account named IAM_USER_NAME'
complete -f -c sre -n '__fish_sre_using_command aws' -a 'enablemfa' -d 'Attach the MFA device with SERIAL_NUMBER to IAM_USER_NAME'
complete -f -c sre -n '__fish_sre_using_command aws' -a 'groups' -d 'List all groups IAM_USER_NAME is a member of'
complete -f -c sre -n '__fish_sre_using_command aws' -a 'list' -d 'List all IAM users in the main account'
complete -f -c sre -n '__fish_sre_using_command aws' -a 'password' -d 'Create a temporary password for IAM_USER_NAME'
complete -f -c sre -n '__fish_sre_using_command aws' -a 'rename' -d 'Rename the IAM user named OLD to NEW'
complete -f -c sre -n '__fish_sre_using_command aws' -a 'rmgroup' -d 'Remove the IAM_USER_NAME from GROUP'
complete -f -c sre -n '__fish_sre_using_command aws' -a 'rmgroups' -d 'Remove the IAM_USER_NAME from provided GROUPs'
complete -f -c sre -n '__fish_sre_using_command aws' -a 'show' -d 'Print IAM_USER_NAME details'
complete -f -c sre -n '__fish_sre_using_command aws' -a 'update' -d 'Apply the configure roles, groups, policies to AWS accounts'

complete -f -c sre -n '__fish_sre_using_command prod' -a 'elbs' -d 'Print important ELB IDs to standard out'
complete -f -c sre -n '__fish_sre_using_command prod' -a 'ips' -d 'Print EC2 public IP addressed to standard out for all instances matching the TAG and VALUE pairs'
complete -f -c sre -n '__fish_sre_using_command prod' -a 'logs' -d 'SSH into all EC2 instances matching the TAG and VALUE pairs and run then "app-logs" command'
complete -f -c sre -n '__fish_sre_using_command prod' -a 'tmux' -d 'Open a tmux session'
complete -f -c sre -n '__fish_sre_using_command prod' -a 'ssh' -d 'Open an SSH session on all matchines matching the TAG and VALUE filters'
complete -f -c sre -n '__fish_sre_using_command prod' -a 'status' -d 'Print the runnind docker image, tag, and container status for all container machines matching the TAG and VALUE filters'
