set -l current_file (basename (status -f))

function read_choice
  set -l target $argv[1]

  while true
    echo "File already exists: $target, what do you want to do?"
    read -l -p read_choice_prompt choice

    switch $choice
      case O
        set -g overwrite_all 1
        return
      case o
        set -g overwrite 1
        return
      case B
        set -g backup_all 1
        return
      case b
        set -g backup 1
        return
      case S
        set -g skip_all 1
        return
      case '' s
        set -g skip 1
        return
    end
  end
end

function read_choice_prompt
  echo '- [s]kip, [S]kip all'
  echo '- [o]verwrite, [O]verwrite all'
  echo '- [b]ackup, [B]ackup all'
  echo '> '
end

read_choice 'test'
exit

set -l target_path $HOME

for file in (ls | cat)
  switch $file
    case $current_file README.md
      continue
    case '*'
      if test -e $target_path/.$file
        echo $target_path/.$file
      else
        echo $target_path/.$file no
      end
  end
end
