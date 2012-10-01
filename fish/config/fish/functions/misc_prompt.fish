function misc_prompt --description 'Write out miscellaneous prompt info'
  set -g misc_prompt
  emit misc_prompt

  for prompt in $misc_prompt
    printf '|'
    printf $prompt
  end
end
