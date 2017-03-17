# helm - is a tool for managing Kubernetes charts. Charts are packages
# of pre-configured Kubernetes resources.
# See: https://github.com/kubernetes/helm

#
# Condition functions
#
function __helm_using_command
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

function __helm_seen_any_subcommand_from -a cmd
    __fish_seen_subcommand_from (__helm_subcommands $cmd | string replace -r '\t.*$' '')
end

#
# Helper functions
#
function __helm_grab_headers
    set -l lines
    while read -l line
        if set -q header
            set lines $lines $line
        else
            set header $line
        end
    end

    set -l positions
    for name in $argv
        set positions $positions (string match -rn -- $name $header | string match -r '\S+')
    end

    for line in $lines
        set -l values
        for pos in $positions
            set values $values (string sub -s $pos -- $line | string match -r '\S+')
        end
        echo $values
    end
end

function __helm_kube_context
    set -l cmd (commandline -p)
    string match -r -- '--kube-context[= ]?\S+' $cmd | string replace -- '--kube-' '--' | string split ' '
end

function __helm_shared_options
    set -l cmd (commandline -p)
    string match -r -- '--kube-context[= ]?\S+' $cmd | string split ' '
end

function __helm_ls
    # TODO: Add caching here for slow/remote clusters.
    helm (__helm_shared_options) ls ^/dev/null | __helm_grab_headers $argv
end

#
# List functions
#
function __helm_kube_contexts
    kubectl config get-contexts -o name ^/dev/null
end

function __helm_kube_namespaces
    kubectl (__helm_kube_context) get namespaces -o name | string replace 'namespace/' ''
end

function __helm_release_revisions
    set -l cmd (commandline -poc)

    for pair in (__helm_ls NAME REVISION)
        echo $pair | read -l release revision

        if contains $release $cmd
            seq 1 $revision
            return
        end
    end
end

function __helm_repositories
    helm repo list ^/dev/null | __helm_grab_headers NAME
end

function __helm_charts
    helm search ^/dev/null | string match -v 'local/*' | __helm_grab_headers NAME
end

function __helm_chart_versions
    set -l cmd (commandline -poc)
    set -l chart

    for i in (__helm_charts)
        if contains $i $cmd
            set chart $i
            break
        end
    end

    helm search $chart -l ^/dev/null | __helm_grab_headers VERSION
end

#
# Completion functions
#
function __helm_subcommands -a cmd
    switch $cmd
        case ''
            echo completion\t'Generate bash autocompletions script'
            echo create\t'Create a new chart with the given name'
            echo delete\t'Delete the release from Kubernetes'
            echo dependency\t"Manage a chart's dependencies"
            echo fetch\t'Download a chart from a repository'
            echo get\t'Download a named release'
            echo history\t'Fetch release history'
            echo home\t'Display the location of HELM_HOME'
            echo init\t'Initialize Helm on both client and server'
            echo inspect\t'Inspect a chart'
            echo install\t'Install a chart archive'
            echo lint\t'Examine a chart for possible issues'
            echo list\t'List releases'
            echo package\t'Package a chart directory into a chart archive'
            echo repo\t'Operate on chart repositories'
            echo reset\t'Uninstalls Tiller from a cluster'
            echo rollback\t'Roll back a release to a previous revision'
            echo search\t'Search for a keyword in charts'
            echo serve\t'Start a local http web server'
            echo status\t'Display the status of the named release'
            echo test\t'Test a release'
            echo upgrade\t'Upgrade a release'
            echo verify\t'Verify that a chart has been signed and is valid'
            echo version\t'Print the client/server version information'
        case 'dependency'
            echo build\t'Rebuild the charts/ directory'
            echo list\t'List the dependencies for the given chart'
            echo update\t'Update charts/'
        case 'get'
            echo hooks\t'Download all hooks for a named release'
            echo manifest\t'Download the manifest for a named release'
            echo values\t'Download the values file for a named release'
        case 'inspect'
            echo chart\t'Show inspect chart'
            echo values\t'Show inspect values'
        case 'repo'
            echo add\t'Add a chart repository'
            echo index\t'Generate an index file'
            echo list\t'List chart repositories'
            echo remove\t'Remove a chart repository'
            echo update\t'Update information on available charts'
    end
end

function __helm_release_completions
    set -l token (commandline -t)
    string match -q -- '-*' $token
    and return

    if contains -- --multiple $argv
        set multiple 1
    end

    set -l releases (__helm_ls NAME CHART)

    if __fish_seen_subcommand_from (string match -r -- '\S+' $releases)
        set -q multiple
        or return
    end

    set -l cmd (commandline -poc)
    for release in $releases
        echo $release | read -l name chart

        contains -- $name $cmd
        or echo $name\t"Release of $chart"
    end
end

function __helm_release_or_chart_completions
    set -l token (commandline -t)
    string match -q -- '-*' $token
    and return

    set -l releases (__helm_ls NAME CHART)

    if not __fish_seen_subcommand_from (string match -r -- '\S+' $releases)
        for release in $releases
            echo $release | read -l name chart
            echo $name\t"Release of $chart"
        end
    else
        set -l charts (__helm_charts)
        __fish_seen_subcommand_from $charts
        and return

        printf '%s\tChart\n' $charts
    end
end

function __helm_release_or_revision_completions
    set -l token (commandline -t)
    string match -q -- '-*' $token
    and return

    set -l releases (__helm_ls NAME CHART REVISION)

    if not __fish_seen_subcommand_from (string match -r -- '\S+' $releases)
        for release in $releases
            echo $release | read -l name chart revision
            echo $name\t"Release of $chart"
        end
    else
        set -l cmd (commandline -poc)
        for release in $releases
            echo $release | read -l name chart revision

            if contains $name $cmd
                set -l revisions (seq 1 $revision)

                __fish_seen_subcommand_from $revisions
                and return

                printf '%s\tRevision\n' $revisions
                return
            end
        end
    end
end

#
# Global Flags
#
complete -c helm -l debug -f -d 'Enable verbose output'
complete -c helm -l home -r -d 'Location of your Helm config'
complete -c helm -l host -x -d 'Address of tiller'
complete -c helm -l kube-context -x -a '(__helm_kube_contexts)' -d 'Name of the kubeconfig context to use'
complete -c helm -l tiller-namespace -x -d 'Namespace of tiller'
complete -c helm -s h -l help -f -d 'More information about a command'

#
# Commands
#

# helm [command]
complete -c helm -n 'not __helm_seen_any_subcommand_from ""' -x -a '(__helm_subcommands "")'

# helm create NAME [flags]
complete -c helm -n '__helm_using_command create' -s p -l starter -x -d 'The named Helm starter scaffold'

# helm delete [flags] RELEASE [...]
complete -c helm -n '__helm_using_command delete' -f -a '(__helm_release_completions --multiple)'

complete -c helm -n '__helm_using_command delete' -l dry-run -f -d 'Simulate a delete'
complete -c helm -n '__helm_using_command delete' -l no-hooks -f -d 'Prevent hooks from running during deletion'
complete -c helm -n '__helm_using_command delete' -l purge -f -d 'Remove the release from the store'
complete -c helm -n '__helm_using_command delete' -l timeout -x -d 'Timeout for kubernetes operations'

# helm dependency [command]
complete -c helm -n '__helm_using_command dependency; and not __helm_seen_any_subcommand_from dependency' -x -a '(__helm_subcommands dependency)'

# helm dependency build [flags] CHART
complete -c helm -n '__helm_using_command dependency build' -l keyring -r -d 'Keyring containing public keys'
complete -c helm -n '__helm_using_command dependency build' -l verify -f -d 'Verify the packages against signatures'

# helm dependency update [flags] CHART
complete -c helm -n '__helm_using_command dependency update' -l keyring -r -d 'Keyring containing public keys'
complete -c helm -n '__helm_using_command dependency update' -l verify -f -d 'Verify the packages against signatures'

# helm fetch [flags] [chart URL | repo/chartname] [...]
complete -c helm -n '__helm_using_command fetch; and not __fish_seen_subcommand_from (__helm_charts)' -f -a '(__helm_charts)' -d 'Chart'

complete -c helm -n '__helm_using_command fetch' -s d -l destination -r -d 'Location to write the chart'
complete -c helm -n '__helm_using_command fetch' -l keyring -r -d 'Keyring containing public keys'
complete -c helm -n '__helm_using_command fetch' -l prov -f -d 'Fetch the provenance file'
complete -c helm -n '__helm_using_command fetch' -l untar -f -d 'Will untar the chart after downloading it'
complete -c helm -n '__helm_using_command fetch --untar' -l untardir -r -d 'Directory into which the chart is expanded'
complete -c helm -n '__helm_using_command fetch' -l verify -f -d 'Verify the package against its signature'
complete -c helm -n '__helm_using_command fetch' -l version -x -a '(__helm_chart_versions)' -d 'Chart version'

# helm get [command]
complete -c helm -n '__helm_using_command get; and not __helm_seen_any_subcommand_from get' -f -a '(__helm_subcommands get)'

# helm get [flags] RELEASE
complete -c helm -n '__helm_using_command get' -f -a '(__helm_release_completions)'

complete -c helm -n '__helm_using_command get' -l revision -x -a '(__helm_release_revisions)' -d 'Revision'

# helm get values [flags] RELEASE
complete -c helm -n '__helm_using_command get values' -s a -l all -f -d 'Dump all (computed) values'

# helm history [flags] RELEASE
complete -c helm -n '__helm_using_command history' -f -a '(__helm_release_completions)'

complete -c helm -n '__helm_using_command history' -l max -x -d 'Maximum number of revision to include in history'

# helm init [flags]
complete -c helm -n '__helm_using_command init' -l canary-image -f -d 'Use the canary tiller image'
complete -c helm -n '__helm_using_command init' -s c -l client-only -f -d 'Do not install tiller'
complete -c helm -n '__helm_using_command init' -l dry-run -f -d 'Do not install local or remote'
complete -c helm -n '__helm_using_command init' -s i -l tiller-image -x -d 'Override tiller image'
complete -c helm -n '__helm_using_command init' -l upgrade -f -d 'Upgrade if tiller is already installed'

# helm inspect [command]
complete -c helm -n '__helm_using_command inspect; and not __helm_seen_any_subcommand_from inspect' -f -a '(__helm_subcommands inspect)'

# helm inspect [CHART] [flags]
complete -c helm -n '__helm_using_command inspect; and not __fish_seen_subcommand_from (__helm_charts)' -a '(__helm_charts)' -d 'Chart'

complete -c helm -n '__helm_using_command inspect' -l keyring -r -d 'Keyring containing public verification keys'
complete -c helm -n '__helm_using_command inspect' -l verify -f -d 'Verify the provenance data for this chart'
complete -c helm -n '__helm_using_command inspect' -l version -x -a '(__helm_chart_versions)' -d 'Chart version'

# helm install [CHART] [flags]
complete -c helm -n '__helm_using_command install; and not __fish_seen_subcommand_from (__helm_charts)' -a '(__helm_charts)' -d 'Chart'

complete -c helm -n '__helm_using_command install' -l dry-run -f -d 'Simulate an install'
complete -c helm -n '__helm_using_command install' -l keyring -r -d 'Keyring containing public verification keys'
complete -c helm -n '__helm_using_command install' -s n -l name -x -d 'Release name'
complete -c helm -n '__helm_using_command install' -l name-template -r -d 'Specify template used to name the release'
complete -c helm -n '__helm_using_command install' -l namespace -x -a '(__helm_kube_namespaces)' -d 'Namespace'
complete -c helm -n '__helm_using_command install' -l no-hooks -f -d 'Prevent hooks from running during install'
complete -c helm -n '__helm_using_command install' -l replace -f -d 'Re-use the given name if already used'
complete -c helm -n '__helm_using_command install' -l set -x -d 'Set values on the command line'
complete -c helm -n '__helm_using_command install' -l timeout -x -d 'Timeout for kubernetes operations'
complete -c helm -n '__helm_using_command install' -s f -l values -r -d 'Specify values in a YAML file'
complete -c helm -n '__helm_using_command install' -l verify -f -d 'Verify the package before installing it'
complete -c helm -n '__helm_using_command install' -l version -x -a '(__helm_chart_versions)' -d 'Chart version'
complete -c helm -n '__helm_using_command install' -l wait -f -d 'Wait until all resources to be ready'

# helm lint [flags] PATH
complete -c helm -n '__helm_using_command lint' -l strict -f -d 'Fail on lint warnings'

# helm list [flags] [FILTER]
complete -c helm -n '__helm_using_command list' -l all -f -d 'Show all releases'
complete -c helm -n '__helm_using_command list' -s d -l date -f -d 'Sort by release date'
complete -c helm -n '__helm_using_command list' -l deleted -f -d 'Show deleted releases'
complete -c helm -n '__helm_using_command list' -l deleting -f -d 'Show releases that are currently being deleted'
complete -c helm -n '__helm_using_command list' -l deployed -f -d 'Show deployed releases'
complete -c helm -n '__helm_using_command list' -l failed -f -d 'Show failed releases'
complete -c helm -n '__helm_using_command list' -s m -l max -x -d 'Maximum number of releases to fetch'
complete -c helm -n '__helm_using_command list' -l namespace -x -a '(__helm_kube_namespaces)' -d 'Show releases within a specific namespace'
complete -c helm -n '__helm_using_command list' -s o -l offset -x -a '(__helm_release_completions)'
complete -c helm -n '__helm_using_command list' -s r -l reverse -f -d 'Reverse the sort order'
complete -c helm -n '__helm_using_command list' -s q -l short -f -d 'Output short listing format'

# helm package [flags] [CHART_PATH] [...]
complete -c helm -n '__helm_using_command package' -l key -x -d 'Name of the key to use when signing'
complete -c helm -n '__helm_using_command package' -l keyring -r -d 'Keyring containing public keys'
complete -c helm -n '__helm_using_command package' -l save -f -d 'Save packaged chart to local chart repository'
complete -c helm -n '__helm_using_command package' -l sign -f -d 'Use a PGP private key to sign this package'
complete -c helm -n '__helm_using_command package' -l version -x -d 'Set the version on the chart to this semver version'

# helm repo [command]
complete -c helm -n '__helm_using_command repo; and not __helm_seen_any_subcommand_from repo' -f -a '(__helm_subcommands repo)'

# helm repo add [flags] [NAME] [URL]
complete -c helm -n '__helm_using_command repo add' -l ca-file -r -d 'CA bundle'
complete -c helm -n '__helm_using_command repo add' -l cert-file -r -d 'Identify HTTPS client using this SSL certificate file'
complete -c helm -n '__helm_using_command repo add' -l key-file -r -d 'Identify HTTPS client using this SSL key file'
complete -c helm -n '__helm_using_command repo add' -l no-update -f -d 'Raise error if repo is already registered'

# helm repo index [flags] [DIR]
complete -c helm -n '__helm_using_command repo index' -l merge -x -d 'Merge the generated index into the given index'
complete -c helm -n '__helm_using_command repo index' -l url -x -d 'URL of chart repository'

# helm repo remove [flags] [NAME]
complete -c helm -n '__helm_using_command repo remove' -f -a '(__helm_repositories)' -d 'Repository'

# helm reset [flags]
complete -c helm -n '__helm_using_command reset' -s f -l force -f -d 'Uninstall even if there are releases installed'
complete -c helm -n '__helm_using_command reset' -l remove-helm-home -f -d 'If set deletes $HELM_HOME'

# helm rollback [RELEASE] [REVISION] [flags]
complete -c helm -n '__helm_using_command rollback' -f -a '(__helm_release_or_revision_completions)'

complete -c helm -n '__helm_using_command rollback' -l dry-run -f -d 'Simulate a rollback'
complete -c helm -n '__helm_using_command rollback' -l no-hooks -f -d 'Prevent hooks from running during rollback'
complete -c helm -n '__helm_using_command rollback' -l recreate-pods -f -d 'Performs pods restart for the resource if applicable'
complete -c helm -n '__helm_using_command rollback' -l timeout -x -d 'Timeout for kubernetes operations'
complete -c helm -n '__helm_using_command rollback' -l wait -f -d 'Wait until all resources to be ready'

# helm search [keyword] [flags]
complete -c helm -n '__helm_using_command search' -s r -l regexp -f -d 'Use regular expressions for searching'
complete -c helm -n '__helm_using_command search' -s l -l versions -f -d 'Show the long listing'

# helm serve [flags]
complete -c helm -n '__helm_using_command serve' -l address -x -d 'Address to listen on'
complete -c helm -n '__helm_using_command serve' -l repo-path -r -d 'Path from which to serve charts'

# helm status [flags] RELEASE
complete -c helm -n '__helm_using_command status' -f -a '(__helm_release_completions)'

complete -c helm -n '__helm_using_command status' -l revision -x -a '(__helm_release_revisions)' -d 'Revision'

# helm test RELEASE [flags]
complete -c helm -n '__helm_using_command test' -f -a '(__helm_release_completions)'

complete -c helm -n '__helm_using_command test' -l cleanup -f -d 'Delete test pods upon completion'
complete -c helm -n '__helm_using_command test' -s t -l timeout -f -d 'Timeout for kubernetes operations'

# helm upgrade [RELEASE] [CHART] [flags]
complete -c helm -n '__helm_using_command upgrade' -f -a '(__helm_release_or_chart_completions)'

complete -c helm -n '__helm_using_command upgrade' -l dry-run -f -d 'Simulate an upgrade'
complete -c helm -n '__helm_using_command upgrade' -s i -l install -f -d "Run an install if the release don't exists"
complete -c helm -n '__helm_using_command upgrade' -l keyring -r -d 'Keyring containing public keys'
complete -c helm -n '__helm_using_command upgrade' -l namespace -x -a '(__helm_kube_namespaces)' -d 'Namespace'
complete -c helm -n '__helm_using_command upgrade' -l no-hooks -f -d 'Disable pre/post upgrade hooks'
complete -c helm -n '__helm_using_command upgrade' -l recreate-pods -f -d 'Performs pods restart for the resource if applicable'
complete -c helm -n '__helm_using_command upgrade' -l reset-values -f -d 'Reset the values to the ones built into the chart'
complete -c helm -n '__helm_using_command upgrade' -l set -x -d 'Set values on the command line'
complete -c helm -n '__helm_using_command upgrade' -l timeout -f -d 'Timeout for kubernetes operations'
complete -c helm -n '__helm_using_command upgrade' -s f -l values -r -d 'Specify values in a YAML file'
complete -c helm -n '__helm_using_command upgrade' -l verify -f -d 'Verify the provenance of the chart before upgrading'
complete -c helm -n '__helm_using_command upgrade' -l version -x -a '(__helm_chart_versions)' -d 'Chart version'
complete -c helm -n '__helm_using_command upgrade' -l wait -f -d 'Wait until all resources to be ready'

# helm verify [flags] PATH
complete -c helm -n '__helm_using_command verify' -l keyring -r -d 'Keyring containing public keys'

# helm version [flags]
complete -c helm -n '__helm_using_command version' -s c -l client -f -d 'Show the client version'
complete -c helm -n '__helm_using_command version' -s s -l server -f -d 'Show the server version'
complete -c helm -n '__helm_using_command version' -l short -f -d 'Print the version number'
