# kubernetes - is an open source system for managing containerized
# applications across multiple hosts, providing basic mechanisms for
# deployment, maintenance, and scaling of applications.
# See: https://kubernetes.io

function __kubectl_using_command
    set -l cmd (commandline -poc)
    set -l found

    test (count $cmd) -gt (count $argv)
    or return 1

    set -e cmd[1]

    for i in $argv
        contains -- $i $cmd
        and set found $found $i
    end

    test "$argv" = "$found"
end

function __kubectl_using_option
    set -l cmd (commandline -poc)
    set -l query '('(string join -- '|' $argv)')'

    if test (count $cmd) -gt 1
        if string match -qr -- $query $cmd[-1]
            return 0
        end
    end
    return 1
end

function __kubectl_using_option_value -a option -a value
    set -l cmd (commandline -poc)

    if test (count $cmd) -gt 1
        string match -qr $option'[= ]'$value -- $cmd
        return $status
    end

    return 1
end

function __kubectl_using_go_template_format
    __kubectl_using_option_value output go-template
    or __kubectl_using_option_value output go-template-file
    or __kubectl_using_option_value o go-template
    or __kubectl_using_option_value o go-template-file
end

function __kubectl_using_resource_type
    for rt in (__kubectl_resource_types)
        if __fish_seen_subcommand_from $rt
            return 0
        end
    end

    return 1
end

function __kubectl_using_resource_prefix
    set -l cmd (commandline -po)
    set -l prefixes (__kubectl_resource_prefixes | string join '|')
    string match -qr '^('$prefixes')/' -- $cmd[-1]
end

function __kubectl_no_pod
    set -l cmd (commandline -pc)
    for i in (__kubectl_pods --no-prefix pods)
        if string match -q "*$i*" -- $cmd
            return 1
        end
    end
    return 0
end

function __kubectl_resource_types
    echo clusters
    echo componentstatuses
    echo configmaps
    echo daemonsets
    echo deployments
    echo endpoints
    echo events
    echo horizontalpodautoscalers
    echo ingresses
    echo jobs
    echo limitranges
    echo namespaces
    echo networkpolicies
    echo nodes
    echo persistentvolumeclaims
    echo persistentvolumes
    echo pods
    echo podsecuritypolicies
    echo podtemplates
    echo replicasets
    echo replicationcontrollers
    echo resourcequotas
    echo secrets
    echo serviceaccounts
    echo services
    echo statefulsets
    echo storageclasses
    echo thirdpartyresources
end

function __kubectl_resource_prefixes
    echo cluster
    echo componentstatus
    echo configmap
    echo daemonset
    echo deployment
    echo endpoint
    echo event
    echo horizontalpodautoscaler
    echo ingress
    echo job
    echo limitrange
    echo namespace
    echo networkpolicy
    echo node
    echo persistentvolumeclaims
    echo persistentvolume
    echo pod
    echo podsecuritypolicy
    echo podtemplate
    echo replicaset
    echo replicationcontroller
    echo resourcequota
    echo secret
    echo serviceaccount
    echo service
    echo statefulset
    echo storageclass
    echo thirdpartyresource
end

function __kubectl_resource_type_description -a type
    switch $type
        case clusters cluster
            echo Cluster
        case componentstatuses componentstatus cs
            echo Status
        case configmaps configmap cm
            echo Config Map
        case daemonsets daemonset ds
            echo Daemon Set
        case deployments deployment deploy
            echo Deployment
        case endpoints endpoint ep
            echo Endpoint
        case events event ev
            echo Event
        case horizontalpodautoscalers horizontalpodautoscaler hpa
            echo Autoscaler
        case ingresses ingress ing
            echo Ingress
        case jobs job
            echo Job
        case limitranges limitrange limits
            echo Range
        case namespaces namespace ns
            echo Namespace
        case networkpolicies networkpolicy
            echo Policy
        case nodes node no
            echo Node
        case persistentvolumeclaims persistentvolumeclaim pvc
            echo Volume Claim
        case persistentvolumes persistentvolume pv
            echo Volume
        case pods pod po
            echo Pod
        case podsecuritypolicies podsecuritypolicy psp
            echo Policy
        case podtemplates podtemplate
            echo Template
        case replicasets replicaset rs
            echo Replica Set
        case replicationcontrollers replicationcontroller rc
            echo RC
        case resourcequotas resourcequota quota
            echo Quota
        case secrets secret
            echo Secret
        case serviceaccounts serviceaccount sa
            echo Account
        case services service svc
            echo Service
        case statefulsets statefulset
            echo Stateful Set
        case storageclasses storageclass
            echo Storage Class
        case thirdpartyresources thirdpartyresource
            echo Resource
    end
end

function __kubectl_resources
    set -l prefix 1

    for i in $argv
        switch $i
            case '--no-prefix'
                set -e prefix
                set idx (contains -i -- --no-prefix $argv)
                set -e argv[$idx]
        end
    end

    set -l cmd (commandline -pc)
    set -l namespace (string replace -r '^kubectl .*(-n |--namespace[= ]?)([^ ]*) .*$' '$2' -- $cmd)

    for resource in $argv
        if set -lq prefix
            kubectl get $resource -n $namespace -o name ^/dev/null
        else
            kubectl get $resource -n $namespace -o name ^/dev/null | string replace -r '.*/' ''
        end
    end
end

function __kubectl_containers
    set -l namespace (string replace -r '^kubectl .*(-n |--namespace[= ]?)([^ ]*) .*$' '$2' -- $argv)
    set -l pod

    for i in (__kubectl_resources --no-prefix pods)
        if string match -q "*$i*" -- $argv
            set pod $i
            break
        end
    end

    if test -z $pod
        return
    end

    kubectl get -n $namespace pods $pod -o 'jsonpath={.spec.containers[*].name}' | string split ' '
end

function __kubectl_pods_completion
    set -l cmd (commandline -pc)
    set -l namespace (string replace -r '^kubectl .*(-n |--namespace[= ]?)([^ ]*) .*$' '$2' -- $cmd)

    kubectl get pods -n $namespace ^/dev/null | tail -n +2 | awk '{print $1"\tPod "$2" "$3}'
end

function __kubectl_complete_resource_subcommand
    set -l resource $argv[-1]
    set -l arguments "(__kubectl_resources $resource | string replace -r '^.*/' '')"
    set -l description (__kubectl_resource_type_description $resource)
    complete -c kubectl -f -n "__kubectl_using_command $argv" -a $arguments -d $description
end

function __kubectl_prefixed_resource_completions
    set -l cmd (commandline -po)

    if string match -qr '[a-zA-Z]+/.*$' -- $cmd[-1]
        set -l type (string replace -r '([a-zA-Z]+)/.*$' '$1' -- $cmd[-1])
        set -l description (__kubectl_resource_type_description $type)
        printf "%s\t$description\n" (__kubectl_resources $type)
    end
end

function __kubectl_explain_field_completions
    set -l cmd (commandline -po)

    string match -qr '[a-zA-Z\.]+\.' -- $cmd[-1]
    or return

    set -l root (string replace -r '([a-zA-Z]+(\.[a-zA-Z])*)\.[a-zA-Z]*$' '$1' -- $cmd[-1])
    set -l section

    for f in (kubectl explain --recursive $root ^/dev/null | sed -n -E '/FIELDS:/,/^[A-Z]*:/ s/^   ([ ]*)([a-zA-Z]+).*$/\1\2/p')
        switch $f
            case ' *'
                echo "$root.$section."(string trim $f)
            case '*'
                set section $f
                echo "$root.$f"
        end
    end
end

function __kubectl_output_formats
    echo json
    echo yaml
    echo wide
    echo name
    echo custom-columns=
    echo custom-columns-file=
    echo go-template=
    echo go-template-file=
    echo jsonpath=
    echo jsonpath-file=
end

function __kubectl_seen_any_subcommand_from -a cmd
    __fish_seen_subcommand_from (__kubectl_subcommands $cmd | string replace -r '\t.*$' '')
end

function __kubectl_subcommands -a cmd
    switch $cmd
        case ''
            # Basic (Beginner)
            echo create\t'Create a resource by filename or stdin'
            echo expose\t'Expose a resource as a new Kubernetes Service'
            echo run\t'Run a particular image on the cluster'
            echo set\t'Set specific features on objects'
            # Basic (Intermediate)
            echo get\t'Display one or many resources'
            echo explain\t'Documentation of resources'
            echo edit\t'Edit a resource on the server'
            echo delete\t'Delete resources'
            # Deploy
            echo rollout\t'Manage a deployment rollout'
            echo rolling-update\t'Perform a rolling update of the given RC'
            echo scale\t'Set a new size for a resource'
            echo autoscale\t'Auto-scale a resource'
            # Cluster Management
            echo certificate\t'Modify certificate resources'
            echo cluster-info\t'Display cluster info'
            echo top\t'Display system resource usage'
            echo cordon\t'Mark node as unschedulable'
            echo uncordon\t'Mark node as schedulable'
            echo drain\t'Drain node in preparation for maintenance'
            echo taint\t'Update the taints on one or more nodes'
            # Troubleshooting and Debugging
            echo describe\t'Show details of a specific resource'
            echo logs\t'Print the logs for a container in a pod'
            echo attach\t'Attach to a running container'
            echo exec\t'Execute a command in a container'
            echo port-forward\t'Forward one or more local ports to a pod'
            echo proxy\t'Run a proxy to the Kubernetes API server'
            echo cp\t'Copy files and directories to and from containers'
            # Advanced
            echo apply\t'Apply a configuration to a resource by filename or stdin'
            echo patch\t'Update field(s) of a resource using strategic merge patch'
            echo replace\t'Replace a resource by filename or stdin'
            echo convert\t'Convert config files between different API versions'
            # Settings
            echo label\t'Update the labels on a resource'
            echo annotate\t'Update the annotations on a resource'
            echo completion\t'Output shell completion code'
            # Other
            echo api-versions\t'Print the supported API versions on the server'
            echo config\t'Modify kubeconfig files'
            echo help\t'Help about any command'
            echo version\t'Print the client and server version information'
            echo options\t'Print the shared options'
        case create
            echo configmap\t'Create a configmap from a local file, directory or literal value'
            echo deployment\t'Create a deployment with the specified name'
            echo namespace\t'Create a namespace with the specified name'
            echo quota\t'Create a quota with the specified name'
            echo secret\t'Create a secret using specified subcommand'
            echo service\t'Create a service using specified subcommand'
            echo serviceaccount\t'Create a service account with the specified name'
        case 'create secret'
            echo docker-registry\t'Create a secret for use with a Docker registry'
            echo generic\t'Create a secret from a local file, directory or literal value'
            echo tls\t'Create a TLS secret'
        case 'create service'
            printf '%s\tService\n' clusterip loadbalancer nodeport
        case expose
            printf '%s\tResource Type\n' pod service replicationcontroller deployment replicaset
        case set
            echo image\t'Update image of a pod template'
            echo resources\t'Update resource requests/limits on objects with pod templates'
        case 'set image'
            printf '%s\tResource Type\n' pod replicationcontroller deployment daemonset job replicaset
        case 'set resources'
            printf '%s\tResource Type\n' replicationcontroller deployment daemonset job replicaset
        case rollout
            echo history\t'View rollout history'
            echo pause\t'Mark the provided resource as paused'
            echo resume\t'Resume a paused resource'
            echo status\t'Show the status of the rollout'
            echo undo\t'Undo a previous rollout'
    end
end

#
# Global command-line options:
#

complete -c kubectl -l alsologtostderr -d 'Log to standard error as well as files'
complete -c kubectl -l as -d 'Username to impersonate for the operation'
complete -c kubectl -l certificate-authority -d 'Path to a cert. file for the certificate authority'
complete -c kubectl -l client-certificate -d 'Path to a client certificate file for TLS'
complete -c kubectl -l client-key -d 'Path to a client key file for TLS'
complete -c kubectl -l cluster -d 'The name of the kubeconfig cluster to use'
complete -c kubectl -l context -d 'The name of the kubeconfig context to use'
complete -c kubectl -l insecure-skip-tls-verify -d 'Certificate will not be checked for validity'
complete -c kubectl -l kubeconfig -d 'Path to the kubeconfig file to use for CLI requests'
complete -c kubectl -l log-backtrace-at -d 'Emit a stack trace when logging hits line file:N'
complete -c kubectl -l log-dir -d 'Write log files in this directory'
complete -c kubectl -l log-flush-frequency -d 'Maximum number of seconds between log flushes'
complete -c kubectl -l logtostderr -d 'Log to standard error instead of files [default true]'
complete -c kubectl -l match-server-version -d 'Require server version to match client version'
complete -c kubectl -s n -l namespace -d 'The namespace scope for this CLI request'
complete -c kubectl -f -n '__kubectl_using_option -n --namespace' -a '(__kubectl_resources --no-prefix namespaces)' -d 'Namespace'
complete -c kubectl -l password -d 'Password for basic authentication to the API server'
complete -c kubectl -l request-timeout -d 'Timeout for a single server request'
complete -c kubectl -s s -l server -d 'The address and port of the Kubernetes API server'
complete -c kubectl -l stderrthreshold -d 'Logs at or above this threshold go to stderr'
complete -c kubectl -l token -d 'Bearer token for authentication to the API server'
complete -c kubectl -l user -d 'The name of the kubeconfig user to use'
complete -c kubectl -l username -d 'Username for basic authentication to the API server'
complete -c kubectl -s v -l v -d 'Log level for V logs'
complete -c kubectl -l vmodule -d 'List of settings for file-filtered logging'
complete -c kubectl -s h -l help -d 'Show more information about a given command'

#
# Basic Commands (Beginner):
#

complete -c kubectl -f -n 'not __kubectl_seen_any_subcommand_from ""' -a '(__kubectl_subcommands "")'

## create
complete -c kubectl -f -n '__kubectl_using_command create; and not __kubectl_seen_any_subcommand_from create' -a '(__kubectl_subcommands create)'

complete -c kubectl -f -n '__kubectl_using_command create' -l dry-run -d 'Only print the object that would be sent'
complete -c kubectl -f -n '__kubectl_using_command create' -l edit -d 'Edit the API resource before creating'
complete -c kubectl -f -n '__kubectl_using_command create; and __kubectl_using_option_value edit true' -l windows-line-endings -d 'Use Windows line-endings'
complete -c kubectl -n '__kubectl_using_command create' -s f -l filename -d 'Filename, directory, or URL to files'
complete -c kubectl -f -n '__kubectl_using_command create; and __kubectl_using_option -f --filename' -s R -l recursive -d 'Process the directory used in -f, --filename recursively'
complete -c kubectl -f -n '__kubectl_using_command create' -l include-extended-apis -d 'Include definitions of new APIs [default true]'
complete -c kubectl -f -n '__kubectl_using_command create' -l no-headers -d "Don't print headers"
complete -c kubectl -f -n '__kubectl_using_command create' -s o -l output -d 'Output format'
complete -c kubectl -f -n '__kubectl_using_command create; and __kubectl_using_option -o --output' -a '(__kubectl_output_formats)' -d 'Format'
complete -c kubectl -f -n '__kubectl_using_command create; and __kubectl_using_go_template_format' -l template -d 'Template string or path to template file to use'
complete -c kubectl -f -n '__kubectl_using_command create' -l output-version -d 'Format object with the given group version'
complete -c kubectl -f -n '__kubectl_using_command create' -l record -d 'Record current kubectl command in the resource annotation'
complete -c kubectl -f -n '__kubectl_using_command create' -l save-config -d 'The config of current object will be saved in its annotation'
complete -c kubectl -f -n '__kubectl_using_command create' -l schema-cache-dir -d 'Load/store cached API schemas in this directory'
complete -c kubectl -f -n '__kubectl_using_command create' -s a -l show-all -d 'When printing, show all resources'
complete -c kubectl -f -n '__kubectl_using_command create' -l show-labels -d 'When printing, show all labels as the last column'
complete -c kubectl -f -n '__kubectl_using_command create' -l sort-by -d 'Sort list types using this field specification'
complete -c kubectl -f -n '__kubectl_using_command create' -l validate -d 'Use a schema to validate the input before sending it [default true]'

### create configmap
complete -c kubectl -f -n '__kubectl_using_command create configmap' -l from-file -d 'File or directory to find config files, with optional key prefix'
complete -c kubectl -f -n '__kubectl_using_command create configmap' -l from-literal -d 'Specify a key and literal value to insert in configmap'
complete -c kubectl -f -n '__kubectl_using_command create configmap' -l generator -d 'The name of the API generator to use'

### create deployment
complete -c kubectl -f -n '__kubectl_using_command create deployment' -l generator -d 'The name of the API generator to use'
complete -c kubectl -f -n '__kubectl_using_command create deployment' -l image -d 'Image name to run'

### create namespace
complete -c kubectl -f -n '__kubectl_using_command create namespace' -l generator -d 'The name of the API generator to use'

### create quota
complete -c kubectl -f -n '__kubectl_using_command create quota' -l generator -d 'The name of the API generator to use'
complete -c kubectl -f -n '__kubectl_using_command create quota' -l hard -d 'A comma-delimited set of resource=quantity pairs'
complete -c kubectl -f -n '__kubectl_using_command create quota' -l scopes -d 'A comma-delimited set of quota scopes'

### create secret
complete -c kubectl -f -n '__kubectl_using_command create secret; and not __kubectl_seen_any_subcommand_from "create secret"' -a '(__kubectl_subcommands "create secret")'

complete -c kubectl -f -n '__kubectl_using_command create secret' -l generator -d 'The name of the API generator to use'

complete -c kubectl -f -n '__kubectl_using_command create secret docker-registry' -l docker-email -d 'Email for Docker registry'
complete -c kubectl -f -n '__kubectl_using_command create secret docker-registry' -l docker-password -d 'Password for Docker registry authentication'
complete -c kubectl -f -n '__kubectl_using_command create secret docker-registry' -l docker-server -d 'Server location for Docker registry'
complete -c kubectl -f -n '__kubectl_using_command create secret docker-registry' -l docker-username -d 'Username for Docker registry authentication'

complete -c kubectl -f -n '__kubectl_using_command create secret generic' -l from-file -d 'File or directory to find config files, with optional key prefix'
complete -c kubectl -f -n '__kubectl_using_command create secret generic' -l from-literal -d 'Specify a key and literal value to insert in configmap'
complete -c kubectl -f -n '__kubectl_using_command create secret generic' -l type -d 'The type of secret to create'

complete -c kubectl -f -n '__kubectl_using_command create secret tls' -l cert -d 'Path to PEM encoded public key certificate'
complete -c kubectl -f -n '__kubectl_using_command create secret tls' -l key -d 'Path to private key associated with given certificate'

### create service
complete -c kubectl -f -n '__kubectl_using_command create service; and not __kubectl_seen_any_subcommand_from "create service"' -a '(__kubectl_subcommands "create service")'

complete -c kubectl -f -n '__kubectl_using_command create service' -l generator -d 'The name of the API generator to use'
complete -c kubectl -f -n '__kubectl_using_command create service' -l tcp -d 'Port pairs can be specified as "<port>:<targetPort>"'

complete -c kubectl -f -n '__kubectl_using_command create service clusterip' -l clusterip -d 'Assign your own ClusterIP or set to "None" for a "headless" service'
complete -c kubectl -f -n '__kubectl_using_command create service nodeport' -l node-port -d 'Port used to expose the service on each node in a cluster'

### create serviceaccount
complete -c kubectl -f -n '__kubectl_using_command create serviceaccount' -l generator -d 'The name of the API generator to use'

## expose
complete -c kubectl -f -n '__kubectl_using_command expose; and not __kubectl_seen_any_subcommand_from expose' -a '(__kubectl_subcommands expose)'

complete -c kubectl -f -n '__kubectl_using_command expose pod' -a '(__kubectl_pods_completion)'

for resource in service replicationcontroller deployment replicaset
    __kubectl_complete_resource_subcommand expose $resource
end

complete -c kubectl -f -n '__kubectl_using_command expose' -l cluster-ip -d 'ClusterIP to be assigned to the service'
complete -c kubectl -f -n '__kubectl_using_command expose' -l container-port -l target-port -d 'Name or number for the port for direct traffic'
complete -c kubectl -f -n '__kubectl_using_command expose' -l create-external-load-balancer -d 'Create an external load balancer for this service'
complete -c kubectl -f -n '__kubectl_using_command expose' -l dry-run -d 'Only print the object that would be sent'
complete -c kubectl -f -n '__kubectl_using_command expose' -l external-ip -d 'Additional external IP address to accept for the service'
complete -c kubectl -n '__kubectl_using_command expose' -s f -l filename -d 'Filename, directory, or URL to files'
complete -c kubectl -f -n '__kubectl_using_command expose; and __kubectl_using_option -f --filename' -s R -l recursive -d 'Process the directory used in -f, --filename recursively'
complete -c kubectl -f -n '__kubectl_using_command expose' -l generator -d 'The name of the API generator to use'
complete -c kubectl -f -n '__kubectl_using_command expose' -s l -l labels -d 'Labels to apply to the service created by this call'
complete -c kubectl -f -n '__kubectl_using_command expose' -l load-balancer-ip -d 'IP to assign to the Load Balancer'
complete -c kubectl -f -n '__kubectl_using_command expose' -l name -d 'The name for the newly created object'
complete -c kubectl -f -n '__kubectl_using_command expose' -l no-headers -d "Don't print headers"
complete -c kubectl -f -n '__kubectl_using_command expose' -l output -d 'Output format'
complete -c kubectl -f -n '__kubectl_using_command expose; and __kubectl_using_option -o --output' -a '(__kubectl_output_formats)' -d 'Format'
complete -c kubectl -f -n '__kubectl_using_command expose; and __kubectl_using_go_template_format' -l template -d 'Template string or path to template file to use'
complete -c kubectl -f -n '__kubectl_using_command expose' -l output-version -d 'Format object with the given group version'
complete -c kubectl -f -n '__kubectl_using_command expose' -l overrides -d 'An inline JSON override for the generated object'
complete -c kubectl -f -n '__kubectl_using_command expose' -l port -d 'The port that the service should serve on'
complete -c kubectl -f -n '__kubectl_using_command expose' -l protocol -d 'The network protocol for the service to be created'
complete -c kubectl -f -n '__kubectl_using_command expose; and __kubectl_using_option --protocol' -a 'TCP UDP' -d 'Protocol'
complete -c kubectl -f -n '__kubectl_using_command expose' -l record -d 'Record current kubectl command in the resource annotation'
complete -c kubectl -f -n '__kubectl_using_command expose' -l save-config -d 'The config of current object will be saved in its annotation'
complete -c kubectl -f -n '__kubectl_using_command expose' -l selector -d 'A label selector to use for this service'
complete -c kubectl -f -n '__kubectl_using_command expose' -l session-affinity -d 'Set the session affinity for the service to this'
complete -c kubectl -f -n '__kubectl_using_command expose; and __kubectl_using_option --session-affinity' -a 'None ClientIP' -d 'Affinity'
complete -c kubectl -f -n '__kubectl_using_command expose' -s a -l show-all -d 'Show all resources'
complete -c kubectl -f -n '__kubectl_using_command expose' -l show-labels -d 'Show all labels as the last column'
complete -c kubectl -f -n '__kubectl_using_command expose' -l sort-by -d 'Sort list types using this field specification'
complete -c kubectl -f -n '__kubectl_using_command expose' -l type -d 'Type for this service'
complete -c kubectl -f -n '__kubectl_using_command expose; and __kubectl_using_option --type' -a 'ClusterIP NodePort LoadBalancer' -d 'Type'

## run
complete -c kubectl -f -n '__kubectl_using_command run' -l attach -d 'Wait for the Pod to start running'
complete -c kubectl -f -n '__kubectl_using_command run' -l command -d 'Use extra arguments as the command field in the container'
complete -c kubectl -f -n '__kubectl_using_command run' -l dry-run -d 'Only print the object that would be sent'
complete -c kubectl -f -n '__kubectl_using_command run' -l env -d 'Environment variables to set in the container'
complete -c kubectl -f -n '__kubectl_using_command run' -l expose -d 'A public, external service is created for the container(s) which are run'
complete -c kubectl -f -n '__kubectl_using_command run; and __kubectl_using_option --expose' -l service-generator -d 'The name of the generator to use for creating a service'
complete -c kubectl -f -n '__kubectl_using_command run; and __kubectl_using_option --expose' -l service-overrides -d 'An inline JSON override for the generated service object'
complete -c kubectl -f -n '__kubectl_using_command run' -l generator -d 'The name of the API generator to use'
complete -c kubectl -f -n '__kubectl_using_command run' -l hostport -d 'The host port mapping for the container port'
complete -c kubectl -f -n '__kubectl_using_command run' -l image -d 'The image for the container to run'
complete -c kubectl -f -n '__kubectl_using_command run' -l image-pull-policy -d 'The image pull policy for the container'
complete -c kubectl -f -n '__kubectl_using_command run' -l include-extended-apis -d 'Include definitions of new APIs [default true]'
complete -c kubectl -f -n '__kubectl_using_command run' -s l -l labels -d 'Labels to apply to the pod(s)'
complete -c kubectl -f -n '__kubectl_using_command run' -l leave-stdin-open -d 'Leave stdin open after the first attach completes'
complete -c kubectl -f -n '__kubectl_using_command run' -l limits -d 'The resource requirement limits for this container'
complete -c kubectl -f -n '__kubectl_using_command run' -l no-headers -d "Don't print headers"
complete -c kubectl -f -n '__kubectl_using_command run' -s o -l output -d 'Output format'
complete -c kubectl -f -n '__kubectl_using_command run; and __kubectl_using_option -o --output' -a '(__kubectl_output_formats)' -d 'Format'
complete -c kubectl -f -n '__kubectl_using_command run; and __kubectl_using_go_template_format' -l template -d 'Template string or path to template file to use'
complete -c kubectl -f -n '__kubectl_using_command run' -l output-version -d 'Format object with the given group version'
complete -c kubectl -f -n '__kubectl_using_command run' -l overrides -d 'An inline JSON override for the generated object'
complete -c kubectl -f -n '__kubectl_using_command run' -l port -d 'The port that this container exposes'
complete -c kubectl -f -n '__kubectl_using_command run' -l quiet -d 'Suppress prompt messages'
complete -c kubectl -f -n '__kubectl_using_command run' -l record -d 'Record current kubectl command in the resource annotation'
complete -c kubectl -f -n '__kubectl_using_command run' -s r -l replicas -d 'Number of replicas to create for this container'
complete -c kubectl -f -n '__kubectl_using_command run' -l requests -d 'The resource requirement requests for this container'
complete -c kubectl -f -n '__kubectl_using_command run' -l restart -d 'The restart policy for this Pod'
complete -c kubectl -f -n '__kubectl_using_command run; and __kubectl_using_option --restart' -a 'Always OnFailure Never' -d 'Policy'
complete -c kubectl -f -n '__kubectl_using_command run' -l rm -d 'Delete resources created in this command for attached containers'
complete -c kubectl -f -n '__kubectl_using_command run' -l save-config -d 'The config of current object will be saved in its annotation'
complete -c kubectl -f -n '__kubectl_using_command run' -l schedule -d 'A schedule in the Cron format the job should be run with'
complete -c kubectl -f -n '__kubectl_using_command run' -s a -l show-all -d 'Show all resources'
complete -c kubectl -f -n '__kubectl_using_command run' -l show-labels -d 'Show all labels as the last column'
complete -c kubectl -f -n '__kubectl_using_command run' -l sort-by -d 'Sort list types using this field specification'
complete -c kubectl -f -n '__kubectl_using_command run' -s i -l stdin -d 'Keep stdin open on the container(s) in the pod'
complete -c kubectl -f -n '__kubectl_using_command run' -s t -l tty -d 'Allocated a TTY for each container in the pod'

## set
complete -c kubectl -f -n '__kubectl_using_command set; and not __kubectl_seen_any_subcommand_from set' -a '(__kubectl_subcommands set)'

### set image
complete -c kubectl -f -n '__kubectl_using_command set image; and not __kubectl_seen_any_subcommand_from "set image"' -a '(__kubectl_subcommands "set image")'

complete -c kubectl -f -n '__kubectl_using_command set image pod' -a '(__kubectl_pods_completion)'

for resource in daemonset deployment job replicaset replicationcontroller
    __kubectl_complete_resource_subcommand set image $resource
end

complete -c kubectl -f -n '__kubectl_using_command set image' -a '(__kubectl_prefixed_resource_completions)'

complete -c kubectl -f -n '__kubectl_using_command set image' -l all -d 'Select all resources in the namespace of the specified resource types'
complete -c kubectl -n '__kubectl_using_command set image' -s f -l filename -d 'Filename, directory, or URL to files'
complete -c kubectl -f -n '__kubectl_using_command set image; and __kubectl_using_option -f --filename' -s R -l recursive -d 'Process the directory used in -f, --filename recursively'
complete -c kubectl -f -n '__kubectl_using_command set image' -l local -d 'Set image will NOT contact api-server but run locally'
complete -c kubectl -f -n '__kubectl_using_command set image' -l no-headers -d "Don't print headers"
complete -c kubectl -f -n '__kubectl_using_command set image' -s o -l output -d 'Output format'
complete -c kubectl -f -n '__kubectl_using_command set image; and __kubectl_using_option -o --output' -a '(__kubectl_output_formats)' -d 'Format'
complete -c kubectl -f -n '__kubectl_using_command set image; and __kubectl_using_go_template_format' -l template -d 'Template string or path to template file to use'
complete -c kubectl -f -n '__kubectl_using_command set image' -l output-version -d 'Format object with the given group version'
complete -c kubectl -f -n '__kubectl_using_command set image' -l record -d 'Record current kubectl command in the resource annotation'
complete -c kubectl -f -n '__kubectl_using_command set image' -s l -l selector -d 'Selector (label query) to filter on'
complete -c kubectl -f -n '__kubectl_using_command set image' -s a -l show-all -d 'Show all resources'
complete -c kubectl -f -n '__kubectl_using_command set image' -l show-labels -d 'Show all labels as the last column'
complete -c kubectl -f -n '__kubectl_using_command set image' -l sort-by -d 'Sort list types using this field specification'

### set resources
complete -c kubectl -f -n '__kubectl_using_command set resources; and not __kubectl_seen_any_subcommand_from "set resources"' -a '(__kubectl_subcommands "set resources")'

for resource in replicationcontroller deployment daemonset job replicaset
    __kubectl_complete_resource_subcommand set resources $resource
end

complete -c kubectl -n '__kubectl_using_command set resources' -l all -d 'Select all resources in the namespace of the specified resource types'
complete -c kubectl -n '__kubectl_using_command set resources' -s c -l containers -d 'The names of containers in the selected pod templates to change'
complete -c kubectl -n '__kubectl_using_command set resources' -l dry-run -d 'Only print the object that would be sent'
complete -c kubectl -n '__kubectl_using_command set resources' -s f -l filename -d 'Filename, directory, or URL to files'
complete -c kubectl -f -n '__kubectl_using_command set resources; and __kubectl_using_option -f --filename' -s R -l recursive -d 'Process the directory used in -f, --filename recursively'
complete -c kubectl -n '__kubectl_using_command set resources' -l limits -d 'The resource requirement requests for this container'
complete -c kubectl -n '__kubectl_using_command set resources' -l local -d 'Set resources will NOT contact api-server but run locally'
complete -c kubectl -n '__kubectl_using_command set resources' -l no-headers -d "Don't print headers"
complete -c kubectl -n '__kubectl_using_command set resources' -s o -l output -d 'Output format'
complete -c kubectl -f -n '__kubectl_using_command set resources; and __kubectl_using_option -o --output' -a '(__kubectl_output_formats)' -d 'Format'
complete -c kubectl -f -n '__kubectl_using_command set resources; and __kubectl_using_go_template_format' -l template -d 'Template string or path to template file to use'
complete -c kubectl -n '__kubectl_using_command set resources' -l output-version -d 'Format object with the given group version'
complete -c kubectl -n '__kubectl_using_command set resources' -l record -d 'Record current kubectl command in the resource annotation'
complete -c kubectl -n '__kubectl_using_command set resources' -l requests -d 'The resource requirement requests for this container'

#
# Basic Commands (Intermediate):
#

## get
complete -c kubectl -f -n '__kubectl_using_command get; and not __kubectl_using_resource_type' -a '(__kubectl_resource_types)' -d 'Resource Type'

for resource in (__kubectl_resource_types)
    __kubectl_complete_resource_subcommand get $resource
end

complete -c kubectl -f -n '__kubectl_using_command get; and not __kubectl_using_resource_type' -a '(__kubectl_prefixed_resource_completions)'

complete -c kubectl -f -n '__kubectl_using_command get' -l all-namespaces -d 'List the requested object(s) across all namespaces'
complete -c kubectl -f -n '__kubectl_using_command get' -l export -d 'Strip cluster-specific info'
complete -c kubectl -f -n '__kubectl_using_command get' -s f -l filename -d 'Filename, directory, or URL to files'
complete -c kubectl -f -n '__kubectl_using_command get; and __kubectl_using_option -f --filename' -s R -l recursive -d 'Process the directory used in -f, --filename recursively'
complete -c kubectl -f -n '__kubectl_using_command get' -l include-extended-apis -d 'Include definitions of new APIs [default true]'
complete -c kubectl -f -n '__kubectl_using_command get' -s L -l label-columns -d 'List of labels to be presented as columns'
complete -c kubectl -f -n '__kubectl_using_command get' -l no-headers -d "Don't print headers"
complete -c kubectl -n '__kubectl_using_command get' -s o -l output -d 'Output format'
complete -c kubectl -f -n '__kubectl_using_command get; and __kubectl_using_option -o --output' -a '(__kubectl_output_formats)' -d 'Format'
complete -c kubectl -f -n '__kubectl_using_command get; and __kubectl_using_go_template_format' -l template -d 'Template string or path to template file to use'
complete -c kubectl -n '__kubectl_using_command get' -l output-version -d 'Format object with the given group version'
complete -c kubectl -n '__kubectl_using_command get' -l raw -d 'Raw URI to request from the server'
complete -c kubectl -n '__kubectl_using_command get' -s l -l selector -d 'Selector (label query) to filter on'
complete -c kubectl -n '__kubectl_using_command get' -s a -l show-all -d 'Show all resources'
complete -c kubectl -n '__kubectl_using_command get' -l show-kind -d 'List the resource type for the requested object(s)'
complete -c kubectl -n '__kubectl_using_command get' -l show-labels -d 'Show all labels as the last column'
complete -c kubectl -n '__kubectl_using_command get' -l sort-by -d 'Sort list types using this field specification'
complete -c kubectl -n '__kubectl_using_command get' -s w -l watch -d 'Watch for changes after listing/getting'
complete -c kubectl -n '__kubectl_using_command get' -l watch-only -d 'Watch for changes to the requested object(s)'

## explain
complete -c kubectl -f -n '__kubectl_using_command explain; and not __kubectl_using_resource_type' -a '(__kubectl_resource_types)' -d 'Resource Type'

complete -c kubectl -f -n '__kubectl_using_command explain' -a '(__kubectl_explain_field_completions)'

complete -c kubectl -f -n '__kubectl_using_command explain' -l include-extended-apis -d 'Include definitions of new APIs [default true]'
complete -c kubectl -f -n '__kubectl_using_command explain' -l recursive -d 'Print the fields of fields'

## edit
complete -c kubectl -f -n '__kubectl_using_command edit; and not __kubectl_using_resource_prefix' -a '(__kubectl_resource_prefixes)/' -d 'Resource Type'
complete -c kubectl -f -n '__kubectl_using_command edit' -a '(__kubectl_prefixed_resource_completions)'

complete -c kubectl -n '__kubectl_using_command edit' -s f -l filename -d 'Filename, directory, or URL to files'
complete -c kubectl -f -n '__kubectl_using_command edit; and __kubectl_using_option -f --filename' -s R -l recursive -d 'Process the directory used in -f, --filename recursively'
complete -c kubectl -f -n '__kubectl_using_command edit' -l include-extended-apis -d 'Include definitions of new APIs [default true]'
complete -c kubectl -f -n '__kubectl_using_command edit' -s o -l output -d 'Output format'
complete -c kubectl -f -n '__kubectl_using_command edit; and __kubectl_using_option -o --output' -a "yaml json" -d "Format"
complete -c kubectl -f -n '__kubectl_using_command edit' -l output-version -d 'Format object with the given group version'
complete -c kubectl -f -n '__kubectl_using_command edit' -l record -d 'Record current kubectl command in the resource annotation'
complete -c kubectl -f -n '__kubectl_using_command edit' -l save-config -d 'The config of current object will be saved in its annotation'
complete -c kubectl -f -n '__kubectl_using_command edit' -l schema-cache-dir -d 'Load/store cached API schemas in this directory'
complete -c kubectl -f -n '__kubectl_using_command edit' -l validate -d 'Use a schema to validate the input before sending'
complete -c kubectl -f -n '__kubectl_using_command edit' -l windows-line-endings -d 'Use Windows line-endings'

## delete
complete -c kubectl -f -n '__kubectl_using_command delete; and not __kubectl_using_resource_type' -a '(__kubectl_resource_types)' -d 'Resource Type'

for resource in (__kubectl_resource_types)
    __kubectl_complete_resource_subcommand delete $resource
end

complete -c kubectl -n '__kubectl_using_command delete' -l all -d 'Select all the specified resources'
complete -c kubectl -n '__kubectl_using_command delete' -l cascade -d 'Cascade the deletion [default true]'
complete -c kubectl -n '__kubectl_using_command delete' -s f -l filename -d 'Filename, directory, or URL to files'
complete -c kubectl -f -n '__kubectl_using_command delete; and __kubectl_using_option -f --filename' -s R -l recursive -d 'Process the directory used in -f, --filename recursively'
complete -c kubectl -n '__kubectl_using_command delete' -l force -d 'Skip confirmation'
complete -c kubectl -n '__kubectl_using_command delete' -l grace-period -d 'Time in seconds for the resource to terminate gracefully'
complete -c kubectl -n '__kubectl_using_command delete' -l ignore-not-found -d 'Treat "resource not found" as a successful delete'
complete -c kubectl -n '__kubectl_using_command delete' -l include-extended-apis -d 'Include definitions of new APIs [default true]'
complete -c kubectl -n '__kubectl_using_command delete' -l now -d 'Resources are signaled for immediate shutdown'
complete -c kubectl -n '__kubectl_using_command delete' -s o -l output -d 'Output mode'
complete -c kubectl -f -n '__kubectl_using_command delete; and __kubectl_using_option -o --output' -a '(__kubectl_output_formats)' -d 'Format'
complete -c kubectl -n '__kubectl_using_command delete' -s l -l selector -d 'Selector (label query) to filter on'
complete -c kubectl -n '__kubectl_using_command delete' -l timeout -d 'Length of time to wait before giving up on a delete'

# Deploy Commands:

## rollout
complete -c kubectl -f -n '__kubectl_using_command rollout; and not __kubectl_seen_any_subcommand_from rollout' -a '(__kubectl_subcommands rollout)'

### rollout history
complete -c kubectl -f -n '__kubectl_using_command rollout history; and not __kubectl_using_resource_type' -a 'deployment' -d 'Resource Type'

for resource in deployment
    __kubectl_complete_resource_subcommand rollout history $resource
end

complete -c kubectl -f -n '__kubectl_using_command rollout history; and not __kubectl_using_resource_type; and not __kubectl_using_resource_prefix' -a 'deployment/' -d 'Deployment...'
complete -c kubectl -f -n '__kubectl_using_command rollout history; and not __kubectl_using_resource_type' -a '(__kubectl_prefixed_resource_completions)'

complete -c kubectl -n '__kubectl_using_command rollout history' -s f -l filename -d 'Filename, directory, or URL to files'
complete -c kubectl -f -n '__kubectl_using_command rollout history; and __kubectl_using_option -f --filename' -s R -l recursive -d 'Process the directory used in -f, --filename recursively'
complete -c kubectl -n '__kubectl_using_command rollout history' -l revision -d 'See the details of the revision specified'

## rollout pause
complete -c kubectl -f -n '__kubectl_using_command rollout pause; and not __kubectl_using_resource_type' -a 'deployment' -d 'Resource Type'

for resource in deployment
    __kubectl_complete_resource_subcommand rollout pause $resource
end

complete -c kubectl -f -n '__kubectl_using_command rollout pause; and not __kubectl_using_resource_type; and not __kubectl_using_resource_prefix' -a 'deployment/' -d 'Deployment...'
complete -c kubectl -f -n '__kubectl_using_command rollout pause; and not __kubectl_using_resource_type' -a '(__kubectl_prefixed_resource_completions)'

complete -c kubectl -n '__kubectl_using_command rollout pause' -s f -l filename -d 'Filename, directory, or URL to files'
complete -c kubectl -f -n '__kubectl_using_command rollout pause; and __kubectl_using_option -f --filename' -s R -l recursive -d 'Process the directory used in -f, --filename recursively'

### rollout resume
complete -c kubectl -f -n '__kubectl_using_command rollout resume; and not __kubectl_using_resource_type' -a 'deployment' -d 'Resource Type'

for resource in deployment
    __kubectl_complete_resource_subcommand rollout resume $resource
end

complete -c kubectl -f -n '__kubectl_using_command rollout resume; and not __kubectl_using_resource_type; and not __kubectl_using_resource_prefix' -a 'deployment/' -d 'Deployment...'
complete -c kubectl -f -n '__kubectl_using_command rollout resume; and not __kubectl_using_resource_type' -a '(__kubectl_prefixed_resource_completions)'

complete -c kubectl -n '__kubectl_using_command rollout resume' -s f -l filename -d 'Filename, directory, or URL to files'
complete -c kubectl -f -n '__kubectl_using_command rollout resume; and __kubectl_using_option -f --filename' -s R -l recursive -d 'Process the directory used in -f, --filename recursively'

### rollout status
complete -c kubectl -f -n '__kubectl_using_command rollout status; and not __kubectl_using_resource_type' -a 'deployment' -d 'Resource Type'

for resource in deployment
    __kubectl_complete_resource_subcommand rollout status $resource
end

complete -c kubectl -f -n '__kubectl_using_command rollout status; and not __kubectl_using_resource_type; and not __kubectl_using_resource_prefix' -a 'deployment/' -d 'Deployment...'
complete -c kubectl -f -n '__kubectl_using_command rollout status; and not __kubectl_using_resource_type' -a '(__kubectl_prefixed_resource_completions)'

complete -c kubectl -n '__kubectl_using_command rollout status' -s f -l filename -d 'Filename, directory, or URL to files'
complete -c kubectl -f -n '__kubectl_using_command rollout status; and __kubectl_using_option -f --filename' -s R -l recursive -d 'Process the directory used in -f, --filename recursively'
complete -c kubectl -n '__kubectl_using_command rollout status' -l revision -d 'Pin to a specific revision for showing its status'
complete -c kubectl -n '__kubectl_using_command rollout status' -s w -l watch -d "Watch the status of the rollout until done"

### rollout undo
complete -c kubectl -f -n '__kubectl_using_command rollout undo; and not __kubectl_using_resource_type' -a 'deployment' -d 'Resource Type'

for resource in deployment
    __kubectl_complete_resource_subcommand rollout undo $resource
end

complete -c kubectl -f -n '__kubectl_using_command rollout undo; and not __kubectl_using_resource_type; and not __kubectl_using_resource_prefix' -a 'deployment/' -d 'Deployment...'
complete -c kubectl -f -n '__kubectl_using_command rollout undo; and not __kubectl_using_resource_type' -a '(__kubectl_prefixed_resource_completions)'

complete -c kubectl -n '__kubectl_using_command rollout undo' -l dry-run -d 'Only print the object that would be sent'
complete -c kubectl -n '__kubectl_using_command rollout undo' -s f -l filename -d 'Filename, directory, or URL to files'
complete -c kubectl -f -n '__kubectl_using_command rollout undo; and __kubectl_using_option -f --filename' -s R -l recursive -d 'Process the directory used in -f, --filename recursively'
complete -c kubectl -n '__kubectl_using_command rollout undo' -l to-revision -d 'The revision to rollback to'

# TODO: Add more commands

# Cluster Management Commands:

# TODO: Add commands

# Troubleshooting and Debugging Commands:

# TODO: Add commands

# Advanced Commands:

# TODO: Add commands

# Settings Commands:

# TODO: Add commands

# Other Commands:

# TODO: Add commands
