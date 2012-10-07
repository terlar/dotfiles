clear_keys() {
  tmux unbind -n "Enter"
  tmux unbind -n "C-c"
  tmux unbind -n "q"
  tmux unbind -t vi-copy "C-t"
  tmux unbind -t vi-copy "C-r"
}

copy_mode() {
  tmux bind -n "Enter" run "$run_self exit_copy_mode"
  tmux bind -n "C-c" run "$run_self cancel_copy_mode"
  tmux bind -n "q" run "$run_self cancel_copy_mode"
  tmux bind -t vi-copy "C-t" copy-selection
  tmux bind -t vi-copy "C-r" cancel
  tmux copy-mode
}
exit_copy_mode() {
  tmux send-keys "C-t"
  clear_keys
  reattach-to-user-namespace -l $SHELL -c "tmux saveb - | pbcopy"
}
cancel_copy_mode() {
  tmux send-keys "C-r"
  clear_keys
}

if [ $# -ne 1 ]; then
  echo "Takes a single argument"
  exit 1
fi

run_self="sh $0"
mode=$1
valid_mode=0

for command in copy_mode exit_copy_mode cancel_copy_mode; do
  if [[ $mode = $command ]]; then
    valid_mode=1
  fi
done

if [[ $valid_mode = 1 ]]; then
  eval $mode
else
  echo "Unrecognized argument"
  exit 1
fi
