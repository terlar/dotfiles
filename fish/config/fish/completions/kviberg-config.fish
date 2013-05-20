# fish completion for kviberg-config
complete -f -c kviberg-config -a import -d 'Imports the database'
complete -f -c kviberg-config -a export -d 'Exports the database'

complete -f -c kviberg-config -l no-xip -d 'Do not generate xip domains'
