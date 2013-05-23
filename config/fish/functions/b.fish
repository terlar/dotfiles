function b
  bundle $argv
  and begin
    test (count $argv) = 0; and sb
    return 0
  end
end
