function fry-reset --description 'Use system ruby'
  set -l new_path

  for i in $PATH
    if echo $i | grep $fry_rubies >/dev/null
      continue
    end

    set new_path $new_path $i
  end

  set PATH $new_path
end
