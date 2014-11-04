function make
	if test -f $PWD/Vagrantfile
		echo 'Running make inside vagrant...'
		vagrant ssh -c "cd /vagrant; make $argv"
	else
		command make $argv
	end
end
