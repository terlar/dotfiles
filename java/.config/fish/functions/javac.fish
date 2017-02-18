function javac
    set classpath (string join : /usr/share/java/**.jar)
    command javac -cp .:src:test:$classpath $argv
end
