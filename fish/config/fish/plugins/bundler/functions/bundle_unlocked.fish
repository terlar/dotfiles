function bundle_unlocked --description 'Unlock local gemfile and run bundle'
  bundle_gemfile unlock
  bundle $argv
end
