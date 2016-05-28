function java
    set classpath (string join : /usr/share/java/**.jar)
    command java -cp .:$classpath $argv
end
