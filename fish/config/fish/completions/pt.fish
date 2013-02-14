# fish completion for pt
function __fish_pt
  not test -e '.pt'    ; and return
  test "$PWD" = "$HOME"; and return

  pt --help | grep pt \
    | sed -e 1d -e 's/^pt //; s/[[:blank:]].*#/'(printf '\t')'/g'
end

complete -f -c pt -a '(__fish_pt)'
