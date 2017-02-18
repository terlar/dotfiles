function junit -a file
    javac $file
    set classpath (string replace -r '(test/)?(.*).java' '$2' "$file" | string replace -a / .)
    java org.junit.runner.JUnitCore $classpath
end
