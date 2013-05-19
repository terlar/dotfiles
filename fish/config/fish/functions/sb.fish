function sb
  which spring >/dev/null; or return

  for i in rspec rake rails
    spring binstub $i
  end
end
