function aws-profile
    set -gx AWS_PROFILE (grep '\[.*\]' ~/.aws/credentials | tr -d '[-]' | fzy)
end
