function docker
	if test -f $PWD/Vagrantfile
		echo 'Running docker inside vagrant...'
		vagrant ssh -c "cd /vagrant; docker $argv"
	else
		command docker $argv
	end
end
