if type -fq direnv
    function __direnv_export_eval --on-event fish_prompt
        eval (direnv export fish)
    end
end
