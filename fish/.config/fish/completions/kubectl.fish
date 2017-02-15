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
    kubectl get namespace -o name | string trim -l -c 'namespace/'
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
    set -l pod

    for i in (__kubectl_resources pods)
        if string match -q "*$i*" -- $argv
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
complete -c kubectl -l logtostderr -f -a 'true false' -d 'Log to standard error instead of files [default true]'
complete -c kubectl -l match-server-version -f -a 'true false' -d 'Require server version to match client version'
complete -c kubectl -s n -l namespace -x -a '(__kubectl_namespaces)' -d 'Namespace'
complete -c kubectl -l password -x -d 'Password for basic authentication to the API server'
complete -c kubectl -l request-timeout -x -d 'Timeout for a single server request'
complete -c kubectl -s s -l server -x -d 'The address and port of the Kubernetes API server'
complete -c kubectl -l stderrthreshold -x -d 'Logs at or above this threshold go to stderr'
complete -c kubectl -l token -x -d 'Bearer token for authentication to the API server'
complete -c kubectl -l user -x -d 'The name of the kubeconfig user'
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

complete -c kubectl -n '__kubectl_using_command create' -l include-extended-apis -f -a 'true false' -d 'Include definitions of new APIs [default true]'
complete -c kubectl -n '__kubectl_using_command create' -l no-headers -f -a 'true false' -d "Don't print headers"

complete -c kubectl -n '__kubectl_using_command create' -s o -l output -x -a '(__kubectl_output_formats)' -d 'Output format'
complete -c kubectl -n '__kubectl_using_command create; and __kubectl_seen_output_with_go_template' -l template -r -d 'Template string or path to template file'

complete -c kubectl -n '__kubectl_using_command create' -l output-version -x -d 'Format object with the given group version'
complete -c kubectl -n '__kubectl_using_command create' -l record -f -a 'true false' -d 'Record current kubectl command in the resource annotation'
complete -c kubectl -n '__kubectl_using_command create' -l save-config -f -a 'true false' -d 'The config of current object will be saved in its annotation'
complete -c kubectl -n '__kubectl_using_command create' -l schema-cache-dir -r -d 'Load/store cached API schemas in this directory'
complete -c kubectl -n '__kubectl_using_command create' -s a -l show-all -f -a 'true false' -d 'When printing, show all resources'
complete -c kubectl -n '__kubectl_using_command create' -l show-labels -f -a 'true false' -d 'When printing, show all labels as the last column'
complete -c kubectl -n '__kubectl_using_command create' -l sort-by -x -d 'Sort list types using this field specification'
complete -c kubectl -n '__kubectl_using_command create' -l validate -f -a 'true false' -d 'Use a schema to validate the input before sending it'

# kubectl create configmap NAME [--from-file=[key=]source] [--from-literal=key1=value1] [--dry-run] [options]
complete -c kubectl -n '__kubectl_using_command create configmap' -l from-file -r -d 'File or directory to find config files, with optional key prefix'
complete -c kubectl -n '__kubectl_using_command create configmap' -l from-literal -x -d 'Specify a key and literal value to insert in configmap'
complete -c kubectl -n '__kubectl_using_command create configmap' -l generator -x -d 'The name of the API generator to use'

# kubectl create deployment NAME --image=image [--dry-run] [options]
complete -c kubectl -n '__kubectl_using_command create deployment' -l generator -x -d 'The name of the API generator to use'
complete -c kubectl -n '__kubectl_using_command create deployment' -l image -x -d 'Image name to run'

# kubectl create namespace NAME [--dry-run] [options]
complete -c kubectl -n '__kubectl_using_command create namespace' -l generator -x -d 'The name of the API generator to use'

# kubectl create quota NAME [--hard=key1=value1,key2=value2] [--scopes=Scope1,Scope2] [--dry-run=bool] [options]
complete -c kubectl -n '__kubectl_using_command create quota' -l generator -x -d 'The name of the API generator to use'
complete -c kubectl -n '__kubectl_using_command create quota' -l hard -x -d 'A comma-delimited set of resource=quantity pairs'
complete -c kubectl -n '__kubectl_using_command create quota' -l scopes -x -d 'A comma-delimited set of quota scopes'

# kubectl create secret [options]
complete -c kubectl -n '__kubectl_using_command create secret; and not __kubectl_seen_any_subcommand_from "create secret"' -f -a '(__kubectl_subcommands "create secret")'

complete -c kubectl -n '__kubectl_using_command create secret' -l generator -x -d 'The name of the API generator to use'

# kubectl create secret docker-registry NAME --docker-username=user --docker-password=password --docker-email=email [--docker-server=string] [--from-literal=key1=value1] [--dry-run] [options]
complete -c kubectl -n '__kubectl_using_command create secret docker-registry' -l docker-email -x -d 'Email for Docker registry'
complete -c kubectl -n '__kubectl_using_command create secret docker-registry' -l docker-password -x -d 'Password for Docker registry authentication'
complete -c kubectl -n '__kubectl_using_command create secret docker-registry' -l docker-server -x -d 'Server location for Docker registry'
complete -c kubectl -n '__kubectl_using_command create secret docker-registry' -l docker-username -x -d 'Username for Docker registry authentication'

# kubectl create secret generic NAME [--type=string] [--from-file=[key=]source] [--from-literal=key1=value1] [--dry-run] [options]
complete -c kubectl -n '__kubectl_using_command create secret generic' -l from-file -r -d 'File or directory to find config files, with optional key prefix'
complete -c kubectl -n '__kubectl_using_command create secret generic' -l from-literal -x -d 'Specify a key and literal value to insert in configmap'
complete -c kubectl -n '__kubectl_using_command create secret generic' -l type -x -d 'The type of secret to create'

# kubectl create secret tls NAME --cert=path/to/cert/file --key=path/to/key/file [--dry-run] [options]
complete -c kubectl -n '__kubectl_using_command create secret tls' -l cert -r -d 'Path to PEM encoded public key certificate'
complete -c kubectl -n '__kubectl_using_command create secret tls' -l key -r -d 'Path to private key associated with given certificate'

# kubectl create service [options]
complete -c kubectl -n '__kubectl_using_command create service; and not __kubectl_seen_any_subcommand_from "create service"' -f -a '(__kubectl_subcommands "create service")'

complete -c kubectl -n '__kubectl_using_command create service' -l generator -x -d 'The name of the API generator to use'
complete -c kubectl -n '__kubectl_using_command create service' -l tcp -x -d 'Port pairs can be specified as "<port>:<targetPort>"'

# kubectl create service clusterip NAME [--tcp=<port>:<targetPort>] [--dry-run] [options]
complete -c kubectl -n '__kubectl_using_command create service clusterip' -l clusterip -x -a 'None' -d 'Assign your own ClusterIP'

# kubectl create service nodeport NAME [--tcp=port:targetPort] [--dry-run] [options]
complete -c kubectl -n '__kubectl_using_command create service nodeport' -l node-port -x -d 'Port used to expose the service on each node in a cluster'

# kubectl create serviceaccount NAME [--dry-run] [options]
complete -c kubectl -n '__kubectl_using_command create serviceaccount' -l generator -x -d 'The name of the API generator to use'

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

complete -c kubectl -n '__kubectl_using_command expose' -l generator -x -d 'The name of the API generator to use'
complete -c kubectl -n '__kubectl_using_command expose' -s l -l labels -x -d 'Labels to apply to the service created by this call'
complete -c kubectl -n '__kubectl_using_command expose' -l load-balancer-ip -x -d 'IP to assign to the Load Balancer'
complete -c kubectl -n '__kubectl_using_command expose' -l name -x -d 'The name for the newly created object'
complete -c kubectl -n '__kubectl_using_command expose' -l no-headers -f -a 'true false' -d "Don't print headers"

complete -c kubectl -n '__kubectl_using_command expose' -s o -l output -x -a '(__kubectl_output_formats)' -d 'Output format'
complete -c kubectl -n '__kubectl_using_command expose; and __kubectl_seen_output_with_go_template' -l template -r -d 'Template string or path to template file'

complete -c kubectl -n '__kubectl_using_command expose' -l output-version -x -d 'Format object with the given group version'
complete -c kubectl -n '__kubectl_using_command expose' -l overrides -x -d 'An inline JSON override for the generated object'
complete -c kubectl -n '__kubectl_using_command expose' -l port -x -d 'The port that the service should serve on'
complete -c kubectl -n '__kubectl_using_command expose' -l protocol -x -a 'TCP UDP' -d 'Network Protocol'
complete -c kubectl -n '__kubectl_using_command expose' -l record -f -a 'true false' -d 'Record current kubectl command in the resource annotation'
complete -c kubectl -n '__kubectl_using_command expose' -l save-config -f -a 'true false' -d 'The config of current object will be saved in its annotation'
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
complete -c kubectl -n '__kubectl_using_command run; and __fish_seen_subcommand_from --expose' -l service-generator -d 'The name of the generator to use for creating a service'
complete -c kubectl -n '__kubectl_using_command run; and __fish_seen_subcommand_from --expose' -l service-overrides -d 'An inline JSON override for the generated service object'
complete -c kubectl -n '__kubectl_using_command run' -l generator -x -d 'The name of the API generator to use'
complete -c kubectl -n '__kubectl_using_command run' -l hostport -x -d 'The host port mapping for the container port'
complete -c kubectl -n '__kubectl_using_command run' -l image -x -d 'The image for the container to run'
complete -c kubectl -n '__kubectl_using_command run' -l image-pull-policy -x -d 'The image pull policy for the container'
complete -c kubectl -n '__kubectl_using_command run' -l include-extended-apis -f -a 'true false' -d 'Include definitions of new APIs [default true]'
complete -c kubectl -n '__kubectl_using_command run' -s l -l labels -x -d 'Labels to apply to the pod(s)'
complete -c kubectl -n '__kubectl_using_command run' -l leave-stdin-open -f -a 'true false' -d 'Leave stdin open after the first attach completes'
complete -c kubectl -n '__kubectl_using_command run' -l limits -x -d 'The resource requirement limits for this container'
complete -c kubectl -n '__kubectl_using_command run' -l no-headers -f -a 'true false' -d "Don't print headers"

complete -c kubectl -n '__kubectl_using_command run' -s o -l output -x -a '(__kubectl_output_formats)' -d 'Output format'
complete -c kubectl -n '__kubectl_using_command run; and __kubectl_seen_output_with_go_template' -l template -r -d 'Template string or path to template file'

complete -c kubectl -n '__kubectl_using_command run' -l output-version -x -d 'Format object with the given group version'
complete -c kubectl -n '__kubectl_using_command run' -l overrides -x -d 'An inline JSON override for the generated object'
complete -c kubectl -n '__kubectl_using_command run' -l port -x -d 'The port that this container exposes'
complete -c kubectl -n '__kubectl_using_command run' -l quiet -f -a 'true false' -d 'Suppress prompt messages'
complete -c kubectl -n '__kubectl_using_command run' -l record -f -a 'true false' -d 'Record current kubectl command in the resource annotation'
complete -c kubectl -n '__kubectl_using_command run' -s r -l replicas -x -d 'Number of replicas to create for this container'
complete -c kubectl -n '__kubectl_using_command run' -l requests -x -d 'The resource requirement requests for this container'
complete -c kubectl -n '__kubectl_using_command run' -l restart -x -a 'Always OnFailure Never' -d 'Restart Policy'
complete -c kubectl -n '__kubectl_using_command run' -l rm -f -a 'true false' -d 'Delete resources created in this command for attached containers'
complete -c kubectl -n '__kubectl_using_command run' -l save-config -f -a 'true false' -d 'The config of current object will be saved in its annotation'
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
complete -c kubectl -n '__kubectl_using_command set resources' -s c -l containers -x -d 'The names of containers in the selected pod templates to change'
complete -c kubectl -n '__kubectl_using_command set resources' -l dry-run -f -a 'true false' -d 'Only print the object that would be sent'

complete -c kubectl -n '__kubectl_using_command set resources' -s f -l filename -r -d 'Filename, directory, or URL to files'
complete -c kubectl -n '__kubectl_using_command set resources; and __fish_seen_subcommand_from -f --filename' -s R -l recursive -f -a 'true false' -d 'Process the directory recursively'

complete -c kubectl -n '__kubectl_using_command set resources' -l limits -x -d 'The resource requirement requests for this container'
complete -c kubectl -n '__kubectl_using_command set resources' -l local -f -a 'true false' -d 'Set resources will NOT contact api-server but run locally'
complete -c kubectl -n '__kubectl_using_command set resources' -l no-headers -f -a 'true false' -d "Don't print headers"

complete -c kubectl -n '__kubectl_using_command set resources' -s o -l output -x -a '(__kubectl_output_formats)' -d 'Output format'
complete -c kubectl -n '__kubectl_using_command set resources; and __kubectl_seen_output_with_go_template' -l template -r -d 'Template string or path to template file'

complete -c kubectl -n '__kubectl_using_command set resources' -l output-version -x -d 'Format object with the given group version'
complete -c kubectl -n '__kubectl_using_command set resources' -l record -f -a 'true false' -d 'Record current kubectl command in the resource annotation'
complete -c kubectl -n '__kubectl_using_command set resources' -l requests -x -d 'The resource requirement requests for this container'

#
# Basic Commands (Intermediate):
#

# kubectl get [(-o|--output=)json|yaml|wide|custom-columns=...|custom-columns-file=...|go-template=...|go-template-file=...|jsonpath=...|jsonpath-file=...] (TYPE [NAME | -l label] | TYPE/NAME ...) [flags] [options]
complete -c kubectl -n '__kubectl_using_command get; and not __kubectl_seen_resource_type' -f -a '(__kubectl_resource_types)' -d 'Resource Type'

for resource in (__kubectl_resource_types)
    __kubectl_complete_resource_subcommand get $resource
end

complete -c kubectl -n '__kubectl_using_command get; and not __kubectl_seen_resource_type' -f -a '(__kubectl_prefixed_resource_completions)'

complete -c kubectl -n '__kubectl_using_command get' -l all-namespaces -f -a 'true false' -d 'List the requested object(s) across all namespaces'
complete -c kubectl -n '__kubectl_using_command get' -l export -f -a 'true false' -d 'Strip cluster-specific info'

complete -c kubectl -n '__kubectl_using_command get' -s f -l filename -r -d 'Filename, directory, or URL to files'
complete -c kubectl -n '__kubectl_using_command get; and __fish_seen_subcommand_from -f --filename' -s R -l recursive -f -a 'true false' -d 'Process the directory recursively'

complete -c kubectl -n '__kubectl_using_command get' -l include-extended-apis -f -a 'true false' -d 'Include definitions of new APIs [default true]'
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

complete -c kubectl -n '__kubectl_using_command edit' -l include-extended-apis -f -a 'true false' -d 'Include definitions of new APIs [default true]'
complete -c kubectl -n '__kubectl_using_command edit' -s o -l output -x -a 'yaml json' -d 'Output format'
complete -c kubectl -n '__kubectl_using_command edit' -l output-version -x -d 'Format object with the given group version'
complete -c kubectl -n '__kubectl_using_command edit' -l record -f -a 'true false' -d 'Record current kubectl command in the resource annotation'
complete -c kubectl -n '__kubectl_using_command edit' -l save-config -f -a 'true false' -d 'The config of current object will be saved in its annotation'
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

# Deploy Commands:

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

complete -c kubectl -n '__kubectl_using_command rollout undo' -l to-revision -x -d 'The revision to rollback to'

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
