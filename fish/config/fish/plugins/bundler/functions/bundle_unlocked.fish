function bundle_unlocked --description 'Unlock local gemfile and run bundle'
  bundle_gemfile unlock
  command bundle $argv
end
