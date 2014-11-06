function fig
	if test -f $PWD/Vagrantfile
		echo 'Running fig inside vagrant...'
		vagrant ssh -c "cd /vagrant; fig $argv"
	else
		command fig $argv
	end
end
