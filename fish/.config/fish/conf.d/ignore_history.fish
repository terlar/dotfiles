function __commandline_ignore_history --on-event fish_preexec
    history --delete fg bg
end
