function fry-ls --description 'List the installed rubies'
  ls -1 $fry_rubies | grep -v current
end
