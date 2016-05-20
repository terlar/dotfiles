### BEGIN GENERATED
function __ag_no_command
    set -l cmd (commandline -poc)
    set -e cmd[1]
    if test (count $cmd) -eq 0
        return 0
    end
    return 1
end

function __ag_using_command
    set cmd (commandline -poc)
    if test (count $cmd) -gt 1
        if test $argv[1] = $cmd[2]
            return 0
        end
    end
    return 1
end

complete -c ag -l "ackmate" -d "Print results in AckMate-parseable format"
complete -c ag -s "A" -d "Print lines after match (Default: 2)"
complete -c ag -l "after" -d "Print lines after match (Default: 2)"
complete -c ag -s "B" -d "Print lines before match (Default: 2)"
complete -c ag -l "before" -d "Print lines before match (Default: 2)"
complete -c ag -s "c" -d "Only print the number of matches in each file."
complete -c ag -l "count" -d "Only print the number of matches in each file."
complete -c ag -l "color-line-number" -d "Color codes for line numbers (Default: 1;33)"
complete -c ag -l "color-match" -d "Color codes for result match numbers (Default: 30;43)"
complete -c ag -l "color-path" -d "Color codes for path names (Default: 1;32)"
complete -c ag -l "column" -d "Print column numbers in results"
complete -c ag -l "heading" -s "H" -d "Print file names before each file's matches"
complete -c ag -s "C" -d "Print lines before and after matches (Default: 2)"
complete -c ag -l "context" -d "Print lines before and after matches (Default: 2)"
complete -c ag -s "g" -d "Print filenames matching PATTERN"
complete -c ag -s "l" -d "Only print filenames that contain matches"
complete -c ag -l "files-with-matches" -d "Only print filenames that contain matches"
complete -c ag -s "L" -d ""
complete -c ag -l "files-without-matches" -d ""
complete -c ag -s "o" -d "Prints only the matching part of the lines"
complete -c ag -l "only-matching" -d "Prints only the matching part of the lines"
complete -c ag -l "print-long-lines" -d "Print matches on very long lines (Default: >2k characters)"
complete -c ag -l "passthrough" -d "When searching a stream, print all lines even if they"
complete -c ag -l "silent" -d "Suppress all log messages, including errors"
complete -c ag -l "stats" -d "Print stats (files scanned, time taken, etc.)"
complete -c ag -l "stats-only" -d "Print stats and nothing else."
complete -c ag -l "vimgrep" -d "Print results like vim's :vimgrep /pattern/g would"
complete -c ag -s "0" -d "Separate filenames with null (for 'xargs -0')"
complete -c ag -l "print0" -d "Separate filenames with null (for 'xargs -0')"
complete -c ag -s "a" -d "Search all files (doesn't include hidden files"
complete -c ag -l "all-types" -d "Search all files (doesn't include hidden files"
complete -c ag -s "D" -d "Ridiculous debugging (probably not useful)"
complete -c ag -l "debug" -d "Ridiculous debugging (probably not useful)"
complete -c ag -l "depth" -d "Search up to NUM directories deep (Default: 25)"
complete -c ag -s "f" -d "Follow symlinks"
complete -c ag -l "follow" -d "Follow symlinks"
complete -c ag -s "F" -d "Alias for --literal for compatibility with grep"
complete -c ag -l "fixed-strings" -d "Alias for --literal for compatibility with grep"
complete -c ag -s "G" -d "Limit search to filenames matching PATTERN"
complete -c ag -l "file-search-regex" -d "Limit search to filenames matching PATTERN"
complete -c ag -l "hidden" -d "Search hidden files (obeys .*ignore files)"
complete -c ag -s "i" -d "Match case insensitively"
complete -c ag -l "ignore-case" -d "Match case insensitively"
complete -c ag -l "ignore" -d "Ignore files/directories matching PATTERN"
complete -c ag -l "ignore-dir" -d "Alias for --ignore for compatibility with ack."
complete -c ag -s "m" -d "Skip the rest of a file after NUM matches (Default: 10,000)"
complete -c ag -l "max-count" -d "Skip the rest of a file after NUM matches (Default: 10,000)"
complete -c ag -l "one-device" -d "Don't follow links to other devices."
complete -c ag -s "p" -d "STRING"
complete -c ag -l "path-to-agignore" -d "STRING"
complete -c ag -s "Q" -d "Don't parse PATTERN as a regular expression"
complete -c ag -l "literal" -d "Don't parse PATTERN as a regular expression"
complete -c ag -s "s" -d "Match case sensitively"
complete -c ag -l "case-sensitive" -d "Match case sensitively"
complete -c ag -s "S" -d "Match case insensitively unless PATTERN contains"
complete -c ag -l "smart-case" -d "Match case insensitively unless PATTERN contains"
complete -c ag -l "search-binary" -d "Search binary files for matches"
complete -c ag -s "t" -d "Search all text files (doesn't include hidden files)"
complete -c ag -l "all-text" -d "Search all text files (doesn't include hidden files)"
complete -c ag -s "u" -d "Search all files (ignore .agignore, .gitignore, etc.;"
complete -c ag -l "unrestricted" -d "Search all files (ignore .agignore, .gitignore, etc.;"
complete -c ag -s "U" -d "Ignore VCS ignore files"
complete -c ag -l "skip-vcs-ignores" -d "Ignore VCS ignore files"
complete -c ag -s "v" -d ""
complete -c ag -l "invert-match" -d ""
complete -c ag -s "w" -d "Only match whole words"
complete -c ag -l "word-regexp" -d "Only match whole words"
complete -c ag -s "W" -d "Truncate match lines after NUM characters"
complete -c ag -l "width" -d "Truncate match lines after NUM characters"
complete -c ag -s "z" -d "Search contents of compressed (e.g., gzip) files"
complete -c ag -l "search-zip" -d "Search contents of compressed (e.g., gzip) files"
### END GENERATED
#
complete -c ag -a "(cut -f 1 tags ^/dev/null | grep -v '!_TAG')"
