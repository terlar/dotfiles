function __terlar_misc_prompt --description 'Write out miscellaneous prompt info'
  set -g misc_prompt
  emit misc_prompt

  for prompt in $misc_prompt
    echo -n '|'
    echo -n $prompt
  end
end
