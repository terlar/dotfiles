function javac
    set classpath (string join : /usr/share/java/*.jar)
    command javac -cp .:$classpath $argv
end
