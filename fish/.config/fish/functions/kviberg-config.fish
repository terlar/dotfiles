function kviberg-config
    cd $farm/kviberg-config

    if test (count $argv) -eq 0
        return 0
    end

    set -l command $argv[1]
    set -e argv[1]
    set -l func_name "kviberg-config-$command"

    if functions -q $func_name
        eval $func_name $argv
    end

    return $status
end

function kviberg-config-export
    echo '==> Dumping database'
    mysqldump -uroot -t --skip-extended-insert --ignore-table=kviberg-config-development.schema_migrations kviberg-config-development >db/export.sql
    ok
    or return 1
end

function kviberg-config-import
    for i in $argv
        switch $i
        end
    end

    echo '==> Recreating database'
    rake db:drop db:create db:schema:load >/dev/null
    ok
    or return 1
    echo '==> Populating database'
    mysql -uroot kviberg-config-development <db/export.sql
    ok
    or return 1

    echo '==> Flushing redis'
    redis-cli flushall
    echo '==> Flushing memcached'
    echo 'flush_all' | telnet 127.0.0.1 11211 >/dev/null ^/dev/null
    ok
    or return 1
end

function ok
    and echo 'OK'
    or begin
        echo 'FAIL'
        return 1
    end
end
