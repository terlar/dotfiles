function cpv --description 'Copy with progress bar'
	rsync -poghb --backup-dir=/tmp/rsync -e /dev/null --progress -- $argv
end
