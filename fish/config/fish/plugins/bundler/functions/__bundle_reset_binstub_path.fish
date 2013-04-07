function __bundle_reset_binstub_path --description 'Remove binstub from path'
  set -l new_path

  for i in $PATH
    switch $i
      case $bundle_binstub_path
        continue
      case '*'
        set new_path $new_path $i
    end
  end

  set -e bundle_binstub_path
  set PATH $new_path
end

