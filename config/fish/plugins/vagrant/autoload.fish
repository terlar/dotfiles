function vmake --wraps make
	vagrant ssh -c "cd /vagrant; make $argv"
end

function vdocker --wraps docker
	vagrant ssh -c "cd /vagrant; docker $argv"
end
