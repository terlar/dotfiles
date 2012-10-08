function prompt_misc --description 'Write out miscellaneous prompt info'
  set -g prompt_misc
  emit prompt_misc

  for prompt in $prompt_misc
    printf '|'
    printf $prompt
  end
end
