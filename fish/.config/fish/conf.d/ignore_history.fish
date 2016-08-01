function __commandline_ignore_history --on-event fish_preexec
    builtin history --delete fg bg
end
