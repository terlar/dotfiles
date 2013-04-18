function e --description 'Execute with environment'
  set -l vars

  for i in $argv
    switch $i
      case '*=*'
        echo $i | sed 's|=| |' | read -l var value
        set vars $vars "set -lx $var $value;"
        set -e argv[1]
      case '*'
        break
    end
  end

  eval "begin; $vars $argv; end"
end
