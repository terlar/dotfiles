function fry-current --description 'Show the current ruby'
  basename (readlink $fry_rubies/current)
end
