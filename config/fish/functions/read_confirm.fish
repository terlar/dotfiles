function read_confirm
  while true
    read -l -p read_confirm_prompt confirm

    switch $confirm
      case Y y
        return 0
      case '' N n
        return 1
    end
  end
end

function read_confirm_prompt
  echo 'Do you want to continue? [Y/n] '
end
