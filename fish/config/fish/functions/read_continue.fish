function read_continue
  while true
    read -l -p read_continue_prompt continue

    switch $continue
      case Y y
        return 0
      case '' N n
        return 1
    end
  end
end

function read_continue_prompt
  echo 'Do you want to continue? [Y/n] '
end
