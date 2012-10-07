zoom_in() {
  zoom_window=zoom@$1
  list=$(tmux list-window)

  if [[ ! $list =~ $zoom_window ]]; then
    tmux new-window -d -n $zoom_window
    tmux swap-pane -s $zoom_window
  fi

  tmux select-window -t $zoom_window
}
zoom_out() {
  zoom_window=$1
  target=${zoom_window:5}
  target_window=${target%%-*}
  target_pane=${target##*-}

  tmux select-window -t $target_window
  tmux select-pane -t $target_pane
  tmux swap-pane -s $zoom_window
  tmux kill-window -t $zoom_window
}
is_zoom() {
  [[ $1 == zoom@* ]]
}

current_window=$(tmux display-message -p '#W')

if is_zoom $current_window; then
  zoom_out $current_window
else
  target=$(tmux display-message -p '#I-#P')
  zoom_in $target
fi
