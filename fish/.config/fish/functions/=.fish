function = --description 'Math wrapper'
    set calc $argv
    set calc (string replace xx '^' $calc)
    set calc (string replace x '*' $calc)
    math "scale=10; $calc"
end
