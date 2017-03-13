# kubernetes - is an open source system for managing containerized
# applications across multiple hosts, providing basic mechanisms for
# deployment, maintenance, and scaling of applications.
# See: https://kubernetes.io

#
# Condition functions
#
function __kubectl_using_command
    set -l cmd (commandline -poc)

    test (count $cmd) -gt (count $argv)
    or return 1
    set -e cmd[1]

    set -l found

    for i in $argv
        contains -- $i $cmd
        and set found $found $i
    end

    test "$argv" = "$found"
end

function __kubectl_using_resource_prefix
    set -l prefixes (__kubectl_resource_prefixes | string join '|')
    string match -qr -- "^($prefixes)/" (commandline -pt)
end

function __kubectl_seen_any_subcommand_from -a cmd
    __fish_seen_subcommand_from (__kubectl_subcommands $cmd | string match -r -- '^\S+')
end

function __kubectl_seen_option_value -a option
    # remove option
    set -e argv[1]

    set -l values '('(string join '|' $argv)')'
    set -l query

    switch $option
        case '--*'
            set query $option'[= ]'$values
        case '-*'
            set query $option' ?'$values
    end

    string match -qr -- $query (commandline -p)
end

function __kubectl_seen_output_with_go_template
    __kubectl_seen_option_value --output go-template go-template-file
    or __kubectl_seen_option_value -o go-template go-template-file
end

function __kubectl_seen_resource_type
    __fish_seen_subcommand_from (__kubectl_resource_types; __kubectl_resource_prefixes)
end

#
# List functions
#
function __kubectl_clusters
    kubectl config get-clusters | tail -n +2
end

function __kubectl_contexts
    kubectl config get-contexts -o name
end

function __kubectl_namespaces
    kubectl (__kubectl_shared_options) get namespace -o name | string replace -r -- '^(\S+)/' ''
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

function __kubectl_resources
    __kubectl_prefixed_resources $argv | string replace -r '^\S*/' ''
end

function __kubectl_prefixed_resources
    set -l opts (__kubectl_shared_options)

    for resource in $argv
        kubectl $opts get $resource -o name ^/dev/null
    end
end

function __kubectl_containers
    set -l cmd (commandline -poc)
    set -l pod

    for i in (__kubectl_resources pods)
        if contains -- $i $cmd
            set pod $i
            break
        end
    end

    if test -z $pod
        return
    end

    kubectl (__kubectl_shared_options) get pods $pod -o 'jsonpath={.spec.containers[*].name}' | string split ' '
end

#
# Helper functions
#
function __kubectl_shared_options
    set -l cmd (commandline -p)
    string match -r -- '--context[= ]?\S+' $cmd | string split ' '
    string match -r -- '-n ?\S+|--namespace[= ]?\S+' $cmd | string split ' '
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

#
# Completion functions
#
function __kubectl_prefixed_resource_completions
    set -l matches (string match -r -- '^(\S+)/' (commandline -pt))

    if set -q matches[2]
        set -l description (__kubectl_resource_type_description $matches[2])
        printf "%s\t$description\n" (__kubectl_prefixed_resources $matches[2])
    end
end

function __kubectl_pod_completions
    kubectl (__kubectl_shared_options) get --no-headers pods ^/dev/null | string replace -r '^(\S+)\s+(\S+)\s+(\S+).*$' '$1\tPod: $2 $3'
end

# Dynamic completions that expand more and more the deeper you
# go. Always completing one extra level of fields.
#
# Examples:
#
# - `resource.` completes `resource.x` and `resource.x.y`
# - `resource.x.` completes `resource.x.y` and `resource.x.y.z`
function __kubectl_explain_field_completions
    set -l token (commandline -pt)

    string match -qr -- '^[\w\.]+\.' $token
    or return

    set -l root (string replace -r '(\w+(\.\w)*)\.\w*$' '$1' -- $token)
    set -l section

    for f in (kubectl explain --recursive $root ^/dev/null | sed -n -E '/FIELDS:/,/^\u*:/ s/^   ([ ]*)(\w+).*$/\1\2/p')
        switch $f
            case ' *'
                echo "$root.$section."(string trim $f)
            case '*'
                set section $f
                echo $root.$f
        end
    end
end

function __kubectl_complete_resource_subcommand
    set -l resource $argv[-1]
    set -l arguments "(__kubectl_resources $resource)"
    set -l description (__kubectl_resource_type_description $resource)
    complete -c kubectl -n "__kubectl_using_command $argv" -f -a $arguments -d $description
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
        case certificate
            echo approve\t'Approve a certificate signing request'
            echo deny\t'Deny a certificate signing request'
        case cluster-info
            echo dump\t'Dump lots of relevant info for debugging and diagnosis'
        case top
            echo node\t'Display resource usage of nodes'
            echo pod\t'Display resource usage of pods'
    end
end

#
# Global command-line options:
#

complete -c kubectl -l alsologtostderr -f -a 'true false' -d 'Log to standard error as well as files'
complete -c kubectl -l as -x -d 'Username to impersonate for the operation'
complete -c kubectl -l certificate-authority -r -d 'Path to a cert. file for the certificate authority'
complete -c kubectl -l client-certificate -r -d 'Path to a client certificate file for TLS'
complete -c kubectl -l client-key -r -d 'Path to a client key file for TLS'
complete -c kubectl -l cluster -x -a '(__kubectl_clusters)' -d 'Cluster'
complete -c kubectl -l context -x -a '(__kubectl_contexts)' -d 'Context'
complete -c kubectl -l insecure-skip-tls-verify -f -a 'true false' -d 'Certificate will not be checked for validity'
complete -c kubectl -l kubeconfig -r -d 'Path to the kubeconfig file to use for CLI requests'
complete -c kubectl -l log-backtrace-at -r -d 'Emit a stack trace when logging hits line file:N'
complete -c kubectl -l log-dir -r -d 'Write log files in this directory'
complete -c kubectl -l log-flush-frequency -x -d 'Maximum number of seconds between log flushes'
complete -c kubectl -l logtostderr -f -a 'true false' -d 'Log to standard error instead of files'
complete -c kubectl -l match-server-version -f -a 'true false' -d 'Require server version to match client version'
complete -c kubectl -s n -l namespace -x -a '(__kubectl_namespaces)' -d 'Namespace'
complete -c kubectl -l password -x -d 'Password for basic authentication to the API server'
complete -c kubectl -l request-timeout -x -d 'Timeout for a single server request'
complete -c kubectl -s s -l server -x -d 'Address and port of the Kubernetes API server'
complete -c kubectl -l stderrthreshold -x -d 'Logs at or above this threshold go to stderr'
complete -c kubectl -l token -x -d 'Bearer token for authentication to the API server'
complete -c kubectl -l user -x -d 'Name of the kubeconfig user'
complete -c kubectl -l username -x -d 'Username for basic authentication to the API server'
complete -c kubectl -s v -l v -x -a '0 1 2 3 4 5' -d 'Log level for V logs'
complete -c kubectl -l vmodule -x -d 'List of settings for file-filtered logging'
complete -c kubectl -s h -l help -d 'Show more information about a given command'

#
# Basic Commands (Beginner):
#

complete -c kubectl -n 'not __kubectl_seen_any_subcommand_from ""' -f -a '(__kubectl_subcommands "")'

# kubectl create -f FILENAME [options]
complete -c kubectl -n '__kubectl_using_command create; and not __kubectl_seen_any_subcommand_from create' -f -a '(__kubectl_subcommands create)'

complete -c kubectl -n '__kubectl_using_command create' -l dry-run -f -a 'true false' -d 'Only print the object that would be sent'
complete -c kubectl -n '__kubectl_using_command create' -l edit -f -a 'true false' -d 'Edit the API resource before creating'
complete -c kubectl -n '__kubectl_using_command create; and __kubectl_seen_option_value --edit true ""' -l windows-line-endings -f -a 'true false' -d 'Use Windows line-endings'

complete -c kubectl -n '__kubectl_using_command create' -s f -l filename -r -d 'Filename, directory, or URL to files'
complete -c kubectl -n '__kubectl_using_command create; and __fish_seen_subcommand_from -f --filename' -s R -l recursive -f -a 'true false' -d 'Process the directory recursively'

complete -c kubectl -n '__kubectl_using_command create' -l include-extended-apis -f -a 'true false' -d 'Include definitions of new APIs'
complete -c kubectl -n '__kubectl_using_command create' -l no-headers -f -a 'true false' -d "Don't print headers"

complete -c kubectl -n '__kubectl_using_command create' -s o -l output -x -a '(__kubectl_output_formats)' -d 'Output format'
complete -c kubectl -n '__kubectl_using_command create; and __kubectl_seen_output_with_go_template' -l template -r -d 'Template string or path to template file'
complete -c kubectl -n '__kubectl_using_command create' -l output-version -x -d 'Format object with the given group version'

complete -c kubectl -n '__kubectl_using_command create' -l record -f -a 'true false' -d 'Record current kubectl command in the resource annotation'
complete -c kubectl -n '__kubectl_using_command create' -l save-config -f -a 'true false' -d 'Config of current object will be saved in its annotation'
complete -c kubectl -n '__kubectl_using_command create' -l schema-cache-dir -r -d 'Load/store cached API schemas in this directory'
complete -c kubectl -n '__kubectl_using_command create' -s a -l show-all -f -a 'true false' -d 'When printing, show all resources'
complete -c kubectl -n '__kubectl_using_command create' -l show-labels -f -a 'true false' -d 'When printing, show all labels as the last column'
complete -c kubectl -n '__kubectl_using_command create' -l sort-by -x -d 'Sort list types using this field specification'
complete -c kubectl -n '__kubectl_using_command create' -l validate -f -a 'true false' -d 'Use a schema to validate the input before sending it'

# kubectl create configmap NAME [--from-file=[key=]source] [--from-literal=key1=value1] [--dry-run] [options]
complete -c kubectl -n '__kubectl_using_command create configmap' -l from-file -r -d 'File or directory to find config files, with optional key prefix'
complete -c kubectl -n '__kubectl_using_command create configmap' -l from-literal -x -d 'Specify a key and literal value to insert in configmap'
complete -c kubectl -n '__kubectl_using_command create configmap' -l generator -x -d 'Name of the API generator to use'

# kubectl create deployment NAME --image=image [--dry-run] [options]
complete -c kubectl -n '__kubectl_using_command create deployment' -l generator -x -d 'Name of the API generator to use'
complete -c kubectl -n '__kubectl_using_command create deployment' -l image -x -d 'Image name to run'

# kubectl create namespace NAME [--dry-run] [options]
complete -c kubectl -n '__kubectl_using_command create namespace' -l generator -x -d 'Name of the API generator to use'

# kubectl create quota NAME [--hard=key1=value1,key2=value2] [--scopes=Scope1,Scope2] [--dry-run=bool] [options]
complete -c kubectl -n '__kubectl_using_command create quota' -l generator -x -d 'Name of the API generator to use'
complete -c kubectl -n '__kubectl_using_command create quota' -l hard -x -d 'A comma-delimited set of resource=quantity pairs'
complete -c kubectl -n '__kubectl_using_command create quota' -l scopes -x -d 'A comma-delimited set of quota scopes'

# kubectl create secret [options]
complete -c kubectl -n '__kubectl_using_command create secret; and not __kubectl_seen_any_subcommand_from "create secret"' -f -a '(__kubectl_subcommands "create secret")'

complete -c kubectl -n '__kubectl_using_command create secret' -l generator -x -d 'Name of the API generator to use'

# kubectl create secret docker-registry NAME --docker-username=user --docker-password=password --docker-email=email [--docker-server=string] [--from-literal=key1=value1] [--dry-run] [options]
complete -c kubectl -n '__kubectl_using_command create secret docker-registry' -l docker-email -x -d 'Email for Docker registry'
complete -c kubectl -n '__kubectl_using_command create secret docker-registry' -l docker-password -x -d 'Password for Docker registry authentication'
complete -c kubectl -n '__kubectl_using_command create secret docker-registry' -l docker-server -x -d 'Server location for Docker registry'
complete -c kubectl -n '__kubectl_using_command create secret docker-registry' -l docker-username -x -d 'Username for Docker registry authentication'

# kubectl create secret generic NAME [--type=string] [--from-file=[key=]source] [--from-literal=key1=value1] [--dry-run] [options]
complete -c kubectl -n '__kubectl_using_command create secret generic' -l from-file -r -d 'File or directory to find config files, with optional key prefix'
complete -c kubectl -n '__kubectl_using_command create secret generic' -l from-literal -x -d 'Specify a key and literal value to insert in configmap'
complete -c kubectl -n '__kubectl_using_command create secret generic' -l type -x -d 'Type of secret to create'

# kubectl create secret tls NAME --cert=path/to/cert/file --key=path/to/key/file [--dry-run] [options]
complete -c kubectl -n '__kubectl_using_command create secret tls' -l cert -r -d 'Path to PEM encoded public key certificate'
complete -c kubectl -n '__kubectl_using_command create secret tls' -l key -r -d 'Path to private key associated with given certificate'

# kubectl create service [options]
complete -c kubectl -n '__kubectl_using_command create service; and not __kubectl_seen_any_subcommand_from "create service"' -f -a '(__kubectl_subcommands "create service")'

complete -c kubectl -n '__kubectl_using_command create service' -l generator -x -d 'Name of the API generator to use'
complete -c kubectl -n '__kubectl_using_command create service' -l tcp -x -d 'Port pairs can be specified as "<port>:<targetPort>"'

# kubectl create service clusterip NAME [--tcp=<port>:<targetPort>] [--dry-run] [options]
complete -c kubectl -n '__kubectl_using_command create service clusterip' -l clusterip -x -a 'None' -d 'Assign your own ClusterIP'

# kubectl create service nodeport NAME [--tcp=port:targetPort] [--dry-run] [options]
complete -c kubectl -n '__kubectl_using_command create service nodeport' -l node-port -x -d 'Port used to expose the service on each node in a cluster'

# kubectl create serviceaccount NAME [--dry-run] [options]
complete -c kubectl -n '__kubectl_using_command create serviceaccount' -l generator -x -d 'Name of the API generator to use'

# kubectl expose (-f FILENAME | TYPE NAME) [--port=port] [--protocol=TCP|UDP] [--target-port=number-or-name] [--name=name] [--external-ip=external-ip-of-service] [--type=type] [options]
complete -c kubectl -n '__kubectl_using_command expose; and not __kubectl_seen_any_subcommand_from expose' -f -a '(__kubectl_subcommands expose)'

complete -c kubectl -n '__kubectl_using_command expose pod' -f -a '(__kubectl_pod_completions)'

for resource in service replicationcontroller deployment replicaset
    __kubectl_complete_resource_subcommand expose $resource
end

complete -c kubectl -n '__kubectl_using_command expose' -l cluster-ip -x -a 'None' -d 'ClusterIP to be assigned to the service'
complete -c kubectl -n '__kubectl_using_command expose' -l container-port -l target-port -x -d 'Name or number for the port for direct traffic'
complete -c kubectl -n '__kubectl_using_command expose' -l create-external-load-balancer -f -a 'true false' -d 'Create an external load balancer for this service'
complete -c kubectl -n '__kubectl_using_command expose' -l dry-run -f -a 'true false' -d 'Only print the object that would be sent'
complete -c kubectl -n '__kubectl_using_command expose' -l external-ip -x -d 'Additional external IP address to accept for the service'

complete -c kubectl -n '__kubectl_using_command expose' -s f -l filename -r -d 'Filename, directory, or URL to files'
complete -c kubectl -n '__kubectl_using_command expose; and __fish_seen_subcommand_from -f --filename' -s R -l recursive -f -a 'true false' -d 'Process the directory recursively'

complete -c kubectl -n '__kubectl_using_command expose' -l generator -x -d 'Name of the API generator to use'
complete -c kubectl -n '__kubectl_using_command expose' -s l -l labels -x -d 'Labels to apply to the service created by this call'
complete -c kubectl -n '__kubectl_using_command expose' -l load-balancer-ip -x -d 'IP to assign to the Load Balancer'
complete -c kubectl -n '__kubectl_using_command expose' -l name -x -d 'Name for the newly created object'
complete -c kubectl -n '__kubectl_using_command expose' -l no-headers -f -a 'true false' -d "Don't print headers"

complete -c kubectl -n '__kubectl_using_command expose' -s o -l output -x -a '(__kubectl_output_formats)' -d 'Output format'
complete -c kubectl -n '__kubectl_using_command expose; and __kubectl_seen_output_with_go_template' -l template -r -d 'Template string or path to template file'
complete -c kubectl -n '__kubectl_using_command expose' -l output-version -x -d 'Format object with the given group version'

complete -c kubectl -n '__kubectl_using_command expose' -l overrides -x -d 'An inline JSON override for the generated object'
complete -c kubectl -n '__kubectl_using_command expose' -l port -x -d 'Port that the service should serve on'
complete -c kubectl -n '__kubectl_using_command expose' -l protocol -x -a 'TCP UDP' -d 'Network Protocol'
complete -c kubectl -n '__kubectl_using_command expose' -l record -f -a 'true false' -d 'Record current kubectl command in the resource annotation'
complete -c kubectl -n '__kubectl_using_command expose' -l save-config -f -a 'true false' -d 'Config of current object will be saved in its annotation'
complete -c kubectl -n '__kubectl_using_command expose' -l selector -x -d 'A label selector to use for this service'
complete -c kubectl -n '__kubectl_using_command expose' -l session-affinity -x -a 'None ClientIP' -d 'Session Affinity'
complete -c kubectl -n '__kubectl_using_command expose' -s a -l show-all -f -a 'true false' -d 'Show all resources'
complete -c kubectl -n '__kubectl_using_command expose' -l show-labels -f -a 'true false' -d 'Show all labels as the last column'
complete -c kubectl -n '__kubectl_using_command expose' -l sort-by -x -d 'Sort list types using this field specification'
complete -c kubectl -n '__kubectl_using_command expose' -l type -x -a 'ClusterIP NodePort LoadBalancer' -d 'Type'

# kubectl run NAME --image=image [--env="key=value"] [--port=port] [--replicas=replicas] [--dry-run=bool] [--overrides=inline-json] [--command] -- [COMMAND] [args...] [options]
complete -c kubectl -n '__kubectl_using_command run' -l attach -f -a 'true false' -d 'Wait for the Pod to start running'
complete -c kubectl -n '__kubectl_using_command run' -l command -f -a 'true false' -d 'Use extra arguments as the command field in the container'
complete -c kubectl -n '__kubectl_using_command run' -l dry-run -f -a 'true false' -d 'Only print the object that would be sent'
complete -c kubectl -n '__kubectl_using_command run' -l env -x -d 'Environment variables to set in the container'
complete -c kubectl -n '__kubectl_using_command run' -l expose -f -a 'true false' -d 'A public, external service is created for the container(s) which are run'
complete -c kubectl -n '__kubectl_using_command run; and __fish_seen_subcommand_from --expose' -l service-generator -d 'Name of the generator to use for creating a service'
complete -c kubectl -n '__kubectl_using_command run; and __fish_seen_subcommand_from --expose' -l service-overrides -d 'An inline JSON override for the generated service object'
complete -c kubectl -n '__kubectl_using_command run' -l generator -x -d 'Name of the API generator to use'
complete -c kubectl -n '__kubectl_using_command run' -l hostport -x -d 'Host port mapping for the container port'
complete -c kubectl -n '__kubectl_using_command run' -l image -x -d 'Image for the container to run'
complete -c kubectl -n '__kubectl_using_command run' -l image-pull-policy -x -d 'Image pull policy for the container'
complete -c kubectl -n '__kubectl_using_command run' -l include-extended-apis -f -a 'true false' -d 'Include definitions of new APIs'
complete -c kubectl -n '__kubectl_using_command run' -s l -l labels -x -d 'Labels to apply to the pod(s)'
complete -c kubectl -n '__kubectl_using_command run' -l leave-stdin-open -f -a 'true false' -d 'Leave stdin open after the first attach completes'
complete -c kubectl -n '__kubectl_using_command run' -l limits -x -d 'Resource requirement limits for this container'
complete -c kubectl -n '__kubectl_using_command run' -l no-headers -f -a 'true false' -d "Don't print headers"

complete -c kubectl -n '__kubectl_using_command run' -s o -l output -x -a '(__kubectl_output_formats)' -d 'Output format'
complete -c kubectl -n '__kubectl_using_command run; and __kubectl_seen_output_with_go_template' -l template -r -d 'Template string or path to template file'
complete -c kubectl -n '__kubectl_using_command run' -l output-version -x -d 'Format object with the given group version'

complete -c kubectl -n '__kubectl_using_command run' -l overrides -x -d 'An inline JSON override for the generated object'
complete -c kubectl -n '__kubectl_using_command run' -l port -x -d 'Port that this container exposes'
complete -c kubectl -n '__kubectl_using_command run' -l quiet -f -a 'true false' -d 'Suppress prompt messages'
complete -c kubectl -n '__kubectl_using_command run' -l record -f -a 'true false' -d 'Record current kubectl command in the resource annotation'
complete -c kubectl -n '__kubectl_using_command run' -s r -l replicas -x -d 'Number of replicas to create for this container'
complete -c kubectl -n '__kubectl_using_command run' -l requests -x -d 'Resource requirement requests for this container'
complete -c kubectl -n '__kubectl_using_command run' -l restart -x -a 'Always OnFailure Never' -d 'Restart Policy'
complete -c kubectl -n '__kubectl_using_command run' -l rm -f -a 'true false' -d 'Delete resources created in this command for attached containers'
complete -c kubectl -n '__kubectl_using_command run' -l save-config -f -a 'true false' -d 'Config of current object will be saved in its annotation'
complete -c kubectl -n '__kubectl_using_command run' -l schedule -x -d 'A schedule in the Cron format the job should be run with'
complete -c kubectl -n '__kubectl_using_command run' -s a -l show-all -f -a 'true false' -d 'Show all resources'
complete -c kubectl -n '__kubectl_using_command run' -l show-labels -f -a 'true false' -d 'Show all labels as the last column'
complete -c kubectl -n '__kubectl_using_command run' -l sort-by -x -d 'Sort list types using this field specification'
complete -c kubectl -n '__kubectl_using_command run' -s i -l stdin -f -a 'true false' -d 'Keep stdin open on the container(s) in the pod'
complete -c kubectl -n '__kubectl_using_command run' -s t -l tty -f -a 'true false' -d 'Allocated a TTY for each container in the pod'

# kubectl set SUBCOMMAND [options]
complete -c kubectl -n '__kubectl_using_command set; and not __kubectl_seen_any_subcommand_from set' -f -a '(__kubectl_subcommands set)'

#  kubectl set image (-f FILENAME | TYPE NAME) CONTAINER_NAME_1=CONTAINER_IMAGE_1 ... CONTAINER_NAME_N=CONTAINER_IMAGE_N [options]
complete -c kubectl -n '__kubectl_using_command set image; and not __kubectl_seen_any_subcommand_from "set image"' -f -a '(__kubectl_subcommands "set image")'

complete -c kubectl -n '__kubectl_using_command set image pod' -f -a '(__kubectl_pod_completions)'

for resource in daemonset deployment job replicaset replicationcontroller
    __kubectl_complete_resource_subcommand set image $resource
end

complete -c kubectl -n '__kubectl_using_command set image' -f -a '(__kubectl_prefixed_resource_completions)'

complete -c kubectl -n '__kubectl_using_command set image' -l all -f -a 'true false' -d 'Select all resources in the namespace of the specified resource types'


complete -c kubectl -n '__kubectl_using_command set image' -s f -l filename -r -d 'Filename, directory, or URL to files'
complete -c kubectl -n '__kubectl_using_command set image; and __fish_seen_subcommand_from -f --filename' -s R -l recursive -f -a 'true false' -d 'Process the directory recursively'

complete -c kubectl -n '__kubectl_using_command set image' -l local -f -a 'true false' -d 'Set image will NOT contact api-server but run locally'
complete -c kubectl -n '__kubectl_using_command set image' -l no-headers -f -a 'true false' -d "Don't print headers"

complete -c kubectl -n '__kubectl_using_command set image' -s o -l output -x -a '(__kubectl_output_formats)' -d 'Output format'
complete -c kubectl -n '__kubectl_using_command set image; and __kubectl_seen_output_with_go_template' -l template -r -d 'Template string or path to template file'
complete -c kubectl -n '__kubectl_using_command set image' -l output-version -x -d 'Format object with the given group version'

complete -c kubectl -n '__kubectl_using_command set image' -l record -f -a 'true false' -d 'Record current kubectl command in the resource annotation'
complete -c kubectl -n '__kubectl_using_command set image' -s l -l selector -x -d 'Selector (label query) to filter on'
complete -c kubectl -n '__kubectl_using_command set image' -s a -l show-all -f -a 'true false' -d 'Show all resources'
complete -c kubectl -n '__kubectl_using_command set image' -l show-labels -f -a 'true false' -d 'Show all labels as the last column'
complete -c kubectl -n '__kubectl_using_command set image' -l sort-by -x -d 'Sort list types using this field specification'

# kubectl set resources (-f FILENAME | TYPE NAME)  ([--limits=LIMITS & --requests=REQUESTS] [options]
complete -c kubectl -n '__kubectl_using_command set resources; and not __kubectl_seen_any_subcommand_from "set resources"' -f -a '(__kubectl_subcommands "set resources")'

for resource in replicationcontroller deployment daemonset job replicaset
    __kubectl_complete_resource_subcommand set resources $resource
end

complete -c kubectl -n '__kubectl_using_command set resources' -l all -f -a 'true false' -d 'Select all resources in the namespace of the specified resource types'
complete -c kubectl -n '__kubectl_using_command set resources' -s c -l containers -x -d 'Names of containers in the selected pod templates to change'
complete -c kubectl -n '__kubectl_using_command set resources' -l dry-run -f -a 'true false' -d 'Only print the object that would be sent'

complete -c kubectl -n '__kubectl_using_command set resources' -s f -l filename -r -d 'Filename, directory, or URL to files'
complete -c kubectl -n '__kubectl_using_command set resources; and __fish_seen_subcommand_from -f --filename' -s R -l recursive -f -a 'true false' -d 'Process the directory recursively'

complete -c kubectl -n '__kubectl_using_command set resources' -l limits -x -d 'Resource requirement requests for this container'
complete -c kubectl -n '__kubectl_using_command set resources' -l local -f -a 'true false' -d 'Set resources will NOT contact api-server but run locally'
complete -c kubectl -n '__kubectl_using_command set resources' -l no-headers -f -a 'true false' -d "Don't print headers"

complete -c kubectl -n '__kubectl_using_command set resources' -s o -l output -x -a '(__kubectl_output_formats)' -d 'Output format'
complete -c kubectl -n '__kubectl_using_command set resources; and __kubectl_seen_output_with_go_template' -l template -r -d 'Template string or path to template file'
complete -c kubectl -n '__kubectl_using_command set resources' -l output-version -x -d 'Format object with the given group version'

complete -c kubectl -n '__kubectl_using_command set resources' -l record -f -a 'true false' -d 'Record current kubectl command in the resource annotation'
complete -c kubectl -n '__kubectl_using_command set resources' -l requests -x -d 'Resource requirement requests for this container'

#
# Basic Commands (Intermediate):
#

# kubectl get [(-o|--output=)json|yaml|wide|custom-columns=...|custom-columns-file=...|go-template=...|go-template-file=...|jsonpath=...|jsonpath-file=...] (TYPE [NAME | -l label] | TYPE/NAME ...) [flags] [options]
complete -c kubectl -n '__kubectl_using_command get; and not __kubectl_seen_resource_type' -f -a '(__kubectl_prefixed_resource_completions)'
complete -c kubectl -n '__kubectl_using_command get; and not __kubectl_seen_resource_type' -f -a '(__kubectl_resource_types)' -d 'Resource Type'

for resource in (__kubectl_resource_types)
    __kubectl_complete_resource_subcommand get $resource
end

complete -c kubectl -n '__kubectl_using_command get' -l all-namespaces -f -a 'true false' -d 'List the requested object(s) across all namespaces'
complete -c kubectl -n '__kubectl_using_command get' -l export -f -a 'true false' -d 'Strip cluster-specific info'

complete -c kubectl -n '__kubectl_using_command get' -s f -l filename -r -d 'Filename, directory, or URL to files'
complete -c kubectl -n '__kubectl_using_command get; and __fish_seen_subcommand_from -f --filename' -s R -l recursive -f -a 'true false' -d 'Process the directory recursively'

complete -c kubectl -n '__kubectl_using_command get' -l include-extended-apis -f -a 'true false' -d 'Include definitions of new APIs'
complete -c kubectl -n '__kubectl_using_command get' -s L -l label-columns -x -d 'List of labels to be presented as columns'
complete -c kubectl -n '__kubectl_using_command get' -l no-headers -f -a 'true false' -d "Don't print headers"

complete -c kubectl -n '__kubectl_using_command get' -s o -l output -x -a '(__kubectl_output_formats)' -d 'Output format'
complete -c kubectl -n '__kubectl_using_command get; and __kubectl_seen_output_with_go_template' -l template -r -d 'Template string or path to template file'
complete -c kubectl -n '__kubectl_using_command get' -l output-version -x -d 'Format object with the given group version'

complete -c kubectl -n '__kubectl_using_command get' -l raw -x -d 'Raw URI to request from the server'
complete -c kubectl -n '__kubectl_using_command get' -s l -l selector -x -d 'Selector (label query) to filter on'
complete -c kubectl -n '__kubectl_using_command get' -s a -l show-all -f -a 'true false' -d 'Show all resources'
complete -c kubectl -n '__kubectl_using_command get' -l show-kind -f -a 'true false' -d 'List the resource type for the requested object(s)'
complete -c kubectl -n '__kubectl_using_command get' -l show-labels -f -a 'true false' -d 'Show all labels as the last column'
complete -c kubectl -n '__kubectl_using_command get' -l sort-by -x -d 'Sort list types using this field specification'
complete -c kubectl -n '__kubectl_using_command get' -s w -l watch -f -a 'true false' -d 'Watch for changes after listing/getting'
complete -c kubectl -n '__kubectl_using_command get' -l watch-only -f -a 'true false' -d 'Watch for changes to the requested object(s)'

# kubectl explain RESOURCE [options]
complete -c kubectl -n '__kubectl_using_command explain; and not __kubectl_seen_resource_type' -f -a '(__kubectl_resource_types)' -d 'Resource Type'

complete -c kubectl -n '__kubectl_using_command explain' -f -a '(__kubectl_explain_field_completions)'

complete -c kubectl -n '__kubectl_using_command explain' -l include-extended-apis -f -a 'true false' -d 'Include definitions of new APIs'
complete -c kubectl -n '__kubectl_using_command explain' -l recursive -f -a 'true false' -d 'Print the fields of fields'

# kubectl edit (RESOURCE/NAME | -f FILENAME) [options]
complete -c kubectl -n '__kubectl_using_command edit; and not __kubectl_using_resource_prefix' -f -a '(__kubectl_resource_prefixes)/' -d 'Resource Type'
complete -c kubectl -n '__kubectl_using_command edit' -f -a '(__kubectl_prefixed_resource_completions)'

complete -c kubectl -n '__kubectl_using_command edit' -s f -l filename -r -d 'Filename, directory, or URL to files'
complete -c kubectl -n '__kubectl_using_command edit; and __fish_seen_subcommand_from -f --filename' -s R -l recursive -f -a 'true false' -d 'Process the directory recursively'

complete -c kubectl -n '__kubectl_using_command edit' -l include-extended-apis -f -a 'true false' -d 'Include definitions of new APIs'

complete -c kubectl -n '__kubectl_using_command edit' -s o -l output -x -a 'yaml json' -d 'Output format'
complete -c kubectl -n '__kubectl_using_command edit' -l output-version -x -d 'Format object with the given group version'

complete -c kubectl -n '__kubectl_using_command edit' -l record -f -a 'true false' -d 'Record current kubectl command in the resource annotation'
complete -c kubectl -n '__kubectl_using_command edit' -l save-config -f -a 'true false' -d 'Config of current object will be saved in its annotation'
complete -c kubectl -n '__kubectl_using_command edit' -l schema-cache-dir -r -d 'Load/store cached API schemas in this directory'
complete -c kubectl -n '__kubectl_using_command edit' -l validate -f -a 'true false' -d 'Use a schema to validate the input before sending'
complete -c kubectl -n '__kubectl_using_command edit' -l windows-line-endings -f -a 'true false' -d 'Use Windows line-endings'

# kubectl delete ([-f FILENAME] | TYPE [(NAME | -l label | --all)]) [options]
complete -c kubectl -n '__kubectl_using_command delete; and not __kubectl_seen_resource_type' -f -a '(__kubectl_resource_types)' -d 'Resource Type'

for resource in (__kubectl_resource_types)
    __kubectl_complete_resource_subcommand delete $resource
end

complete -c kubectl -n '__kubectl_using_command delete' -l all -f -a 'true false' -d 'Select all the specified resources'
complete -c kubectl -n '__kubectl_using_command delete' -l cascade -f -a 'true false' -d 'Cascade the deletion'

complete -c kubectl -n '__kubectl_using_command delete' -s f -l filename -r -d 'Filename, directory, or URL to files'
complete -c kubectl -n '__kubectl_using_command delete; and __fish_seen_subcommand_from -f --filename' -s R -l recursive -f -a 'true false' -d 'Process the directory recursively'

complete -c kubectl -n '__kubectl_using_command delete' -l force -f -a 'true false' -d 'Skip confirmation'
complete -c kubectl -n '__kubectl_using_command delete' -l grace-period -x -d 'Time in seconds for the resource to terminate gracefully'
complete -c kubectl -n '__kubectl_using_command delete' -l ignore-not-found -f -a 'true false' -d 'Treat "resource not found" as a successful delete'
complete -c kubectl -n '__kubectl_using_command delete' -l include-extended-apis -f -a 'true false' -d 'Include definitions of new APIs'
complete -c kubectl -n '__kubectl_using_command delete' -l now -f -a 'true false' -d 'Resources are signaled for immediate shutdown'
complete -c kubectl -n '__kubectl_using_command delete' -s o -l output -x -a '(__kubectl_output_formats)' -d 'Output format'
complete -c kubectl -n '__kubectl_using_command delete' -s l -l selector -x -d 'Selector (label query) to filter on'
complete -c kubectl -n '__kubectl_using_command delete' -l timeout -x -d 'Length of time to wait before giving up on a delete'

#
# Deploy Commands:
#

# kubectl rollout SUBCOMMAND [options]
complete -c kubectl -n '__kubectl_using_command rollout; and not __kubectl_seen_any_subcommand_from rollout' -f -a '(__kubectl_subcommands rollout)'

# kubectl rollout history (TYPE NAME | TYPE/NAME) [flags] [options]
complete -c kubectl -n '__kubectl_using_command rollout history; and not __kubectl_seen_resource_type' -f -a 'deployment' -d 'Resource Type'

for resource in deployment
    __kubectl_complete_resource_subcommand rollout history $resource
end

complete -c kubectl -n '__kubectl_using_command rollout history; and not __kubectl_seen_resource_type; and not __kubectl_using_resource_prefix' -f -a 'deployment/' -d 'Deployment...'
complete -c kubectl -n '__kubectl_using_command rollout history; and not __kubectl_seen_resource_type' -f -a '(__kubectl_prefixed_resource_completions)'

complete -c kubectl -n '__kubectl_using_command rollout history' -s f -l filename -r -d 'Filename, directory, or URL to files'
complete -c kubectl -n '__kubectl_using_command rollout history; and __fish_seen_subcommand_from -f --filename' -s R -l recursive -f -a 'true false' -d 'Process the directory recursively'

complete -c kubectl -n '__kubectl_using_command rollout history' -l revision -x -d 'See the details of the revision specified'

# kubectl rollout pause RESOURCE [options]
complete -c kubectl -n '__kubectl_using_command rollout pause; and not __kubectl_seen_resource_type' -f -a 'deployment' -d 'Resource Type'

for resource in deployment
    __kubectl_complete_resource_subcommand rollout pause $resource
end

complete -c kubectl -n '__kubectl_using_command rollout pause; and not __kubectl_seen_resource_type; and not __kubectl_using_resource_prefix' -f -a 'deployment/' -d 'Deployment...'
complete -c kubectl -n '__kubectl_using_command rollout pause; and not __kubectl_seen_resource_type' -f -a '(__kubectl_prefixed_resource_completions)'

complete -c kubectl -n '__kubectl_using_command rollout pause' -s f -l filename -r -d 'Filename, directory, or URL to files'
complete -c kubectl -n '__kubectl_using_command rollout pause; and __fish_seen_subcommand_from -f --filename' -s R -l recursive -f -a 'true false' -d 'Process the directory recursively'

# kubectl rollout resume RESOURCE [options]
complete -c kubectl -n '__kubectl_using_command rollout resume; and not __kubectl_seen_resource_type' -f -a 'deployment' -d 'Resource Type'

for resource in deployment
    __kubectl_complete_resource_subcommand rollout resume $resource
end

complete -c kubectl -n '__kubectl_using_command rollout resume; and not __kubectl_seen_resource_type; and not __kubectl_using_resource_prefix' -f -a 'deployment/' -d 'Deployment...'
complete -c kubectl -n '__kubectl_using_command rollout resume; and not __kubectl_seen_resource_type' -f -a '(__kubectl_prefixed_resource_completions)'

complete -c kubectl -n '__kubectl_using_command rollout resume' -s f -l filename -r -d 'Filename, directory, or URL to files'
complete -c kubectl -n '__kubectl_using_command rollout resume; and __fish_seen_subcommand_from -f --filename' -s R -l recursive -f -a 'true false' -d 'Process the directory recursively'

# kubectl rollout status (TYPE NAME | TYPE/NAME) [flags] [options]
complete -c kubectl -n '__kubectl_using_command rollout status; and not __kubectl_seen_resource_type' -f -a 'deployment' -d 'Resource Type'

for resource in deployment
    __kubectl_complete_resource_subcommand rollout status $resource
end

complete -c kubectl -n '__kubectl_using_command rollout status; and not __kubectl_seen_resource_type; and not __kubectl_using_resource_prefix' -f -a 'deployment/' -d 'Deployment...'
complete -c kubectl -n '__kubectl_using_command rollout status; and not __kubectl_seen_resource_type' -f -a '(__kubectl_prefixed_resource_completions)'

complete -c kubectl -n '__kubectl_using_command rollout status' -s f -l filename -r -d 'Filename, directory, or URL to files'
complete -c kubectl -n '__kubectl_using_command rollout status; and __fish_seen_subcommand_from -f --filename' -s R -l recursive -f -a 'true false' -d 'Process the directory recursively'

complete -c kubectl -n '__kubectl_using_command rollout status' -l revision -x -d 'Pin to a specific revision for showing its status'
complete -c kubectl -n '__kubectl_using_command rollout status' -s w -l watch -f -a 'true false' -d "Watch the status of the rollout until done"

# kubectl rollout undo (TYPE NAME | TYPE/NAME) [flags] [options]
complete -c kubectl -n '__kubectl_using_command rollout undo; and not __kubectl_seen_resource_type' -f -a 'deployment' -d 'Resource Type'

for resource in deployment
    __kubectl_complete_resource_subcommand rollout undo $resource
end

complete -c kubectl -n '__kubectl_using_command rollout undo; and not __kubectl_seen_resource_type; and not __kubectl_using_resource_prefix' -f -a 'deployment/' -d 'Deployment...'
complete -c kubectl -n '__kubectl_using_command rollout undo; and not __kubectl_seen_resource_type' -f -a '(__kubectl_prefixed_resource_completions)'

complete -c kubectl -n '__kubectl_using_command rollout undo' -l dry-run -f -a 'true false' -d 'Only print the object that would be sent'

complete -c kubectl -n '__kubectl_using_command rollout undo' -s f -l filename -r -d 'Filename, directory, or URL to files'
complete -c kubectl -n '__kubectl_using_command rollout undo; and __fish_seen_subcommand_from -f --filename' -s R -l recursive -f -a 'true false' -d 'Process the directory recursively'

complete -c kubectl -n '__kubectl_using_command rollout undo' -l to-revision -x -d 'Revision to rollback to'

# kubectl rolling-update OLD_CONTROLLER_NAME ([NEW_CONTROLLER_NAME] --image=NEW_CONTAINER_IMAGE | -f NEW_CONTROLLER_SPEC) [options]
# TODO: Complete controller name?

complete -c kubectl -n '__kubectl_using_command rolling-update' -l allow-missing-template-keys -f -a 'true false' -d 'Ignore errors in templates when a key is missing'
complete -c kubectl -n '__kubectl_using_command rolling-update' -l dry-run -f -a 'true false' -d 'Only print the object that would be sent'
complete -c kubectl -n '__kubectl_using_command rolling-update' -s f -l filename -r -d 'Filename or URL to file for replication controller'

complete -c kubectl -n '__kubectl_using_command rolling-update' -l image -x -d 'Image to use for upgrading the replication controller'
complete -c kubectl -n '__kubectl_using_command rolling-update; and __fish_seen_subcommand_from --image' -l container -x -d 'Container name which will have its image upgraded'
complete -c kubectl -n '__kubectl_using_command rolling-update; and __fish_seen_subcommand_from --image' -l deployment-label-key -x -d 'Key to use to differentiate between two different controllers'
complete -c kubectl -n '__kubectl_using_command rolling-update; and __fish_seen_subcommand_from --image' -l image-pull-policy -x -d 'Explicit policy for when to pull container images'

complete -c kubectl -n '__kubectl_using_command rolling-update' -l include-extended-apis -f -a 'true false' -d 'Include definitions of new APIs'
complete -c kubectl -n '__kubectl_using_command rolling-update' -l no-headers -f -a 'true false' -d "Don't print headers"

complete -c kubectl -n '__kubectl_using_command rolling-update' -s o -l output -x -a '(__kubectl_output_formats)' -d 'Output format'
complete -c kubectl -n '__kubectl_using_command rolling-update; and __kubectl_seen_output_with_go_template' -l template -r -d 'Template string or path to template file'
complete -c kubectl -n '__kubectl_using_command rolling-update' -l output-version -x -d 'Format object with the given group version'

complete -c kubectl -n '__kubectl_using_command rolling-update' -l poll-interval -x -d 'Time delay between polling'
complete -c kubectl -n '__kubectl_using_command rolling-update' -l rollback -f -a 'true false' -d 'Abort an existing rollout that is partially rolled out'
complete -c kubectl -n '__kubectl_using_command rolling-update' -l schema-cache-dir -r -d 'Load/store cached API schemas in this directory'
complete -c kubectl -n '__kubectl_using_command rolling-update' -s a -l show-all -f -a 'true false' -d 'When printing, show all resources'
complete -c kubectl -n '__kubectl_using_command rolling-update' -l show-labels -f -a 'true false' -d 'When printing, show all labels as the last column'
complete -c kubectl -n '__kubectl_using_command rolling-update' -l sort-by -x -d 'Sort list types using this field specification'
complete -c kubectl -n '__kubectl_using_command rolling-update' -l timeout -x -d 'Length of time to wait before giving up'
complete -c kubectl -n '__kubectl_using_command rolling-update' -l update-period -x -d 'Time to wait between updating pods'
complete -c kubectl -n '__kubectl_using_command rolling-update' -l validate -f -a 'true false' -d 'Use a schema to validate the input before sending it'

# kubectl scale [--resource-version=version] [--current-replicas=count] --replicas=COUNT (-f FILENAME | TYPE/NAME) [options]
complete -c kubectl -n '__kubectl_using_command scale; and not __kubectl_using_resource_prefix' -f -a 'deployment/ replicaset/ replicationcontroller/ job/' -d 'Resource Type'
complete -c kubectl -n '__kubectl_using_command scale' -f -a '(__kubectl_prefixed_resource_completions)'

complete -c kubectl -n '__kubectl_using_command scale' -l current-replicas -x -d 'Precondition for current size'

complete -c kubectl -n '__kubectl_using_command scale' -s f -l filename -r -d 'Filename, directory, or URL to files'
complete -c kubectl -n '__kubectl_using_command scale; and __fish_seen_subcommand_from -f --filename' -s R -l recursive -f -a 'true false' -d 'Process the directory recursively'

complete -c kubectl -n '__kubectl_using_command scale' -l include-extended-apis -f -a 'true false' -d 'Include definitions of new APIs'
complete -c kubectl -n '__kubectl_using_command scale' -s o -l output -x -a 'name' -d 'Output format'
complete -c kubectl -n '__kubectl_using_command scale' -l record -f -a 'true false' -d 'Record current kubectl command in the resource annotation'
complete -c kubectl -n '__kubectl_using_command scale' -l replicas -x -d 'New desired number of replicas'
complete -c kubectl -n '__kubectl_using_command scale' -l resource-version -x -d 'Precondition for resource version'
complete -c kubectl -n '__kubectl_using_command scale' -l timeout -x -d 'Length of time to wait before giving up'

# kubectl autoscale (-f FILENAME | TYPE NAME | TYPE/NAME) [--min=MINPODS] --max=MAXPODS [--cpu-percent=CPU] [flags] [options]
complete -c kubectl -n '__kubectl_using_command autoscale' -f -a '(__kubectl_prefixed_resource_completions)'
complete -c kubectl -n '__kubectl_using_command autoscale; and not __kubectl_seen_resource_type' -f -a 'deployment replicaset replicationcontroller' -d 'Resource Type'

for resource in deployment replicaset replicationcontroller
    __kubectl_complete_resource_subcommand autoscale $resource
end

complete -c kubectl -n '__kubectl_using_command autoscale' -l allow-missing-template-keys -f -a 'true false' -d 'Ignore errors in templates when a key is missing'
complete -c kubectl -n '__kubectl_using_command autoscale' -l cpu-percent -x -d 'Target average CPU utilization'
complete -c kubectl -n '__kubectl_using_command autoscale' -l dry-run -f -a 'true false' -d 'Only print the object that would be sent'

complete -c kubectl -n '__kubectl_using_command autoscale' -s f -l filename -r -d 'Filename, directory, or URL to files'
complete -c kubectl -n '__kubectl_using_command autoscale; and __fish_seen_subcommand_from -f --filename' -s R -l recursive -f -a 'true false' -d 'Process the directory recursively'

complete -c kubectl -n '__kubectl_using_command autoscale' -l generator -x -a 'horizontalpodautoscaler/v1' -d 'Name of the API generator to use'
complete -c kubectl -n '__kubectl_using_command autoscale' -l include-extended-apis -f -a 'true false' -d 'Include definitions of new APIs'
complete -c kubectl -n '__kubectl_using_command autoscale' -l max -x -d 'Upper limit for the number of pods'
complete -c kubectl -n '__kubectl_using_command autoscale' -l min -x -d 'Lower limit for the number of pods'
complete -c kubectl -n '__kubectl_using_command autoscale' -l name -x -d 'Name for the newly created object'
complete -c kubectl -n '__kubectl_using_command autoscale' -l no-headers -f -a 'true false' -d "Don't print headers"

complete -c kubectl -n '__kubectl_using_command autoscale' -s o -l output -x -a '(__kubectl_output_formats)' -d 'Output format'
complete -c kubectl -n '__kubectl_using_command autoscale; and __kubectl_seen_output_with_go_template' -l template -r -d 'Template string or path to template file'
complete -c kubectl -n '__kubectl_using_command autoscale' -l output-version -x -d 'Format object with the given group version'

complete -c kubectl -n '__kubectl_using_command autoscale' -l record -f -a 'true false' -d 'Record current kubectl command in the resource annotation'
complete -c kubectl -n '__kubectl_using_command autoscale' -l save-config -f -a 'true false' -d 'Configuration of current object will be saved in its annotation'
complete -c kubectl -n '__kubectl_using_command autoscale' -s a -l show-all -f -a 'true false' -d 'When printing, show all resources'
complete -c kubectl -n '__kubectl_using_command autoscale' -l show-labels -f -a 'true false' -d 'When printing, show all labels as the last column'
complete -c kubectl -n '__kubectl_using_command autoscale' -l sort-by -x -d 'Sort list types using this field specification'

#
# Cluster Management Commands:
#

# kubectl certificate SUBCOMMAND [options]
complete -c kubectl -n '__kubectl_using_command certificate; and not __kubectl_seen_any_subcommand_from certificate' -f -a '(__kubectl_subcommands certificate)'

complete -c kubectl -n '__kubectl_using_command certificate; and __kubectl_seen_any_subcommand_from certificate' -s f -l filename -r -d 'Filename, directory, or URL to files'
complete -c kubectl -n '__kubectl_using_command certificate; and __kubectl_seen_any_subcommand_from certificate; and __fish_seen_subcommand_from -f --filename' -s R -l recursive -f -a 'true false' -d 'Process the directory recursively'

complete -c kubectl -n '__kubectl_using_command certificate; and __kubectl_seen_any_subcommand_from certificate' -s o -l output -x -a 'name' -d 'Output format'

# kubectl cluster-info [options]
complete -c kubectl -n '__kubectl_using_command cluster-info; and not __kubectl_seen_any_subcommand_from cluster-info' -f -a '(__kubectl_subcommands cluster-info)'

complete -c kubectl -n '__kubectl_using_command cluster-info' -l include-extended-apis -f -a 'true false' -d 'Include definitions of new APIs'

# kubectl cluster-info dump [options]
complete -c kubectl -n '__kubectl_using_command cluster-info dump' -l all-namespaces -f -a 'true false' -d 'Dump all namespaces'
complete -c kubectl -n '__kubectl_using_command cluster-info dump' -l namespaces -x -d 'Comma separated list of namespaces to dump'
complete -c kubectl -n '__kubectl_using_command cluster-info dump' -l output-directory -r -d 'Where to output the files'

# kubectl top [options]
complete -c kubectl -n '__kubectl_using_command top; and not __kubectl_seen_any_subcommand_from top' -f -a '(__kubectl_subcommands top)'

# kubectl top node [NAME | -l label] [options]
complete -c kubectl -n '__kubectl_using_command top node' -f -a '(__kubectl_resources node)' -d 'Node'
complete -c kubectl -n '__kubectl_using_command top node' -s l -l selector -x -d 'Selector to filter on'

# kubectl top pod [NAME | -l label] [options]
complete -c kubectl -n '__kubectl_using_command top pod' -f -a '(__kubectl_pod_completions)'

complete -c kubectl -n '__kubectl_using_command top pod' -l all-namespaces -f -a 'true false' -d 'List the requested object(s) across all namespaces'
complete -c kubectl -n '__kubectl_using_command top pod' -l containers -f -a 'true false' -d 'Print usage of containers within a pod'
complete -c kubectl -n '__kubectl_using_command top pod' -s l -l selector -x -d 'Selector to filter on'

# kubectl cordon NODE [options]
complete -c kubectl -n '__kubectl_using_command cordon' -f -a '(__kubectl_resources node)' -d 'Node'

# kubectl uncordon NODE [options]
complete -c kubectl -n '__kubectl_using_command uncordon' -f -a '(__kubectl_resources node)' -d 'Node'

# kubectl drain NODE [options]
complete -c kubectl -n '__kubectl_using_command drain' -f -a '(__kubectl_resources node)' -d 'Node'

complete -c kubectl -n '__kubectl_using_command drain' -l delete-local-data -f -a 'true false' -d 'Continue even if there are pods using emptyDir'
complete -c kubectl -n '__kubectl_using_command drain' -l force -f -a 'true false' -d 'Continue even if there are pods not managed'
complete -c kubectl -n '__kubectl_using_command drain' -l grace-period -x -d 'Time given to each pod to terminate gracefully'
complete -c kubectl -n '__kubectl_using_command drain' -l ignore-daemonsets -f -a 'true false' -d 'Ignore DaemonSet-managed pods'
complete -c kubectl -n '__kubectl_using_command drain' -l timeout -x -d 'Length of time to wait before giving up'

# kubectl taint NODE NAME KEY_1=VAL_1:TAINT_EFFECT_1 ... KEY_N=VAL_N:TAINT_EFFECT_N [options]
complete -c kubectl -n '__kubectl_using_command taint; and not __kubectl_seen_resource_type' -f -a 'nodes' -d 'Resource Type'

for resource in nodes
    __kubectl_complete_resource_subcommand taint $resource
end

complete -c kubectl -n '__kubectl_using_command taint' -l all -f -a 'true false' -d 'Select all nodes in the cluster'
complete -c kubectl -n '__kubectl_using_command taint' -l allow-missing-template-keys -f -a 'true false' -d 'Ignore errors in templates when a key is missing'
complete -c kubectl -n '__kubectl_using_command taint' -l include-extended-apis -f -a 'true false' -d 'Include definitions of new APIs'
complete -c kubectl -n '__kubectl_using_command taint' -l no-headers -f -a 'true false' -d "Don't print headers"

complete -c kubectl -n '__kubectl_using_command taint' -s o -l output -x -a '(__kubectl_output_formats)' -d 'Output format'
complete -c kubectl -n '__kubectl_using_command taint; and __kubectl_seen_output_with_go_template' -l template -r -d 'Template string or path to template file'
complete -c kubectl -n '__kubectl_using_command taint' -l output-version -x -d 'Format object with the given group version'

complete -c kubectl -n '__kubectl_using_command taint' -l overwrite -f -a 'true false' -d 'Allow taints to be overwritten'
complete -c kubectl -n '__kubectl_using_command taint' -l schema-cache-dir -r -d 'Load/store cached API schemas in this directory'
complete -c kubectl -n '__kubectl_using_command taint' -s l -l selector -x -d 'Selector to filter on'
complete -c kubectl -n '__kubectl_using_command taint' -s a -l show-all -f -a 'true false' -d 'When printing, show all resources'
complete -c kubectl -n '__kubectl_using_command taint' -l show-labels -f -a 'true false' -d 'When printing, show all labels as the last column'
complete -c kubectl -n '__kubectl_using_command taint' -l sort-by -x -d 'Sort list types using this field specification'
complete -c kubectl -n '__kubectl_using_command taint' -l validate -f -a 'true false' -d 'Use a schema to validate the input before sending it'

#
# Troubleshooting and Debugging Commands:
#

# kubectl describe (-f FILENAME | TYPE [NAME_PREFIX | -l label] | TYPE/NAME) [options]
complete -c kubectl -n '__kubectl_using_command describe; and not __kubectl_seen_resource_type' -f -a '(__kubectl_prefixed_resource_completions)'
complete -c kubectl -n '__kubectl_using_command describe; and not __kubectl_seen_resource_type' -f -a '(__kubectl_resource_types)' -d 'Resource Type'

for resource in (__kubectl_resource_types)
    __kubectl_complete_resource_subcommand describe $resource
end

complete -c kubectl -n '__kubectl_using_command describe' -l all-namespaces -f -a 'true false' -d 'List the requested object(s) across all namespaces'

complete -c kubectl -n '__kubectl_using_command describe' -s f -l filename -r -d 'Filename, directory, or URL to files'
complete -c kubectl -n '__kubectl_using_command describe; and __fish_seen_subcommand_from -f --filename' -s R -l recursive -f -a 'true false' -d 'Process the directory recursively'

complete -c kubectl -n '__kubectl_using_command describe' -l include-extended-apis -f -a 'true false' -d 'Include definitions of new APIs'
complete -c kubectl -n '__kubectl_using_command describe' -s l -l selector -x -d 'Selector to filter on'
complete -c kubectl -n '__kubectl_using_command describe' -l show-events -f -a 'true false' -d 'Display events related to the described object'

# kubectl logs [-f] [-p] POD [-c CONTAINER] [options]
complete -c kubectl -n '__kubectl_using_command logs' -f -a '(__kubectl_pod_completions)'

complete -c kubectl -n '__kubectl_using_command logs' -s c -l container -x -a '(__kubectl_containers)' -d 'Container'
complete -c kubectl -n '__kubectl_using_command logs' -s f -l follow -f -a 'true false' -d 'Follow logs'
complete -c kubectl -n '__kubectl_using_command logs' -l include-extended-apis -f -a 'true false' -d 'Include definitions of new APIs'
complete -c kubectl -n '__kubectl_using_command logs' -l interactive -f -a 'true false' -d 'Prompt the user for input when required'
complete -c kubectl -n '__kubectl_using_command logs' -l limit-bytes -x -d 'Maximum bytes of logs to return'
complete -c kubectl -n '__kubectl_using_command logs' -s p -l previous -f -a 'true false' -d 'Target the previous instance of container'
complete -c kubectl -n '__kubectl_using_command logs; and not __fish_seen_subcommand_from --since-time' -l since -x -d 'Only return logs newer than a relative duration'
complete -c kubectl -n '__kubectl_using_command logs; and not __fish_seen_subcommand_from --since' -l since-time -x -d 'Only return logs after a specific date'
complete -c kubectl -n '__kubectl_using_command logs' -l tail -x -d 'Lines of recent log file to display'
complete -c kubectl -n '__kubectl_using_command logs' -l timestamps -f -a 'true false' -d 'Include timestamps on each line in the log output'

# kubectl attach POD -c CONTAINER [options]
complete -c kubectl -n '__kubectl_using_command attach' -f -a '(__kubectl_pod_completions)'

complete -c kubectl -n '__kubectl_using_command attach' -s c -l container -x -a '(__kubectl_containers)' -d 'Container'
complete -c kubectl -n '__kubectl_using_command attach' -s i -l stdin -f -a 'true false' -d 'Pass stdin to the container'
complete -c kubectl -n '__kubectl_using_command attach' -s t -l tty -f -a 'true false' -d 'Stdin is a TTY'

# TODO: Add commands

#
# Advanced Commands:
#

# TODO: Add commands

#
# Settings Commands:
#

# TODO: Add commands

#
# Other Commands:
#

# TODO: Add commands
