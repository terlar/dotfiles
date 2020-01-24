function nix-shell
    if test (count $argv) -eq 0
        command nix-shell --command fish
    else
        command nix-shell $argv
    end
end
