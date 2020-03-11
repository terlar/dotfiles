function aws-profile-export -a profile
    test -n "$profile" && set -gx AWS_PROFILE "$profile"
    set -gx AWS_ACCESS_KEY_ID (aws configure get aws_access_key_id)
    set -gx AWS_SECRET_ACCESS_KEY (aws configure get aws_secret_access_key)
    set -gx AWS_SECURITY_TOKEN (aws configure get aws_security_token)
    set -gx AWS_SESSION_TOKEN (aws configure get aws_session_token)
end
