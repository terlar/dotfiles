set brew_completions_file '/usr/local/Library/Contributions/brew_fish_completion.fish'

if test -f $brew_completions_file
  . $brew_completions_file
end
