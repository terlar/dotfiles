# menu - menu command interface wrapper
# This command works either with rofi or fzf depending on which one
# exist and if it is a graphical environment or not.

complete -c menu -x -a '(__fish_complete_command)'
