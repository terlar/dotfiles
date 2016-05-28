function java
    set classpath (string join : /usr/share/java/**.jar)
    command java -cp .:src:test:$classpath $argv
end
