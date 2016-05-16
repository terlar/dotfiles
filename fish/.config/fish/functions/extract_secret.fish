function extract_secret
    echo $argv | read -l key section
    for line in (pass show $key)
        string match -qr "^$section:" $line; or continue
        string replace -r '[a-z_]+: (.+)' '$1' $line
        return
    end
end
