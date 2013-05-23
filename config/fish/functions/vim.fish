function vim
  if which reattach-to-user-namespace >/dev/null
    reattach-to-user-namespace command vim $argv
    return
  end

  command vim $argv
end
