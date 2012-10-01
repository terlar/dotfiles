# fish completion for bundle_gemfile
function __fish_bundle_gemfile
  ls | grep -v 'Gemfile$\|.lock$'
end
complete -f -c bgem -a '(__fish_bundle_gemfile)'
