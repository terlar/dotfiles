function nix-shell
    if not string match -qr '.*--run|--command.*' -- $argv
        set argv --command fish $argv
    end

    command nix-shell $argv
end
