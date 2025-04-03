tmp=`mktemp -d /tmp/testcc-test-XXXXXX`
trap 'rm -rf $tmp' INT TERM HUP EXIT

"$FILE" --help | grep astron -q

if [ ! $? -eq 0 ]; then
    echo 'please set FILE to Fine Free File Command from https://www.darwinsys.com/file'
    exit 1
fi

if [ -z "$CC" ]; then
    echo "please set CC"
    exit 1
fi

check() {
    if [ $? -eq 0 ]; then
        echo "testing $1 ... passed"
    else
        echo "testing $1 ... failed"
        exit 1
    fi
}

echo 'extern int bar; int foo() { return bar; }' > $tmp/foo.c
echo 'int foo(); int bar=3; int main() { foo(); }' > $tmp/bar.c

test_obj() {
    $CC $1 -o $tmp/foo $tmp/foo.c $tmp/bar.c
    check "$1 build"

    STR=`$FILE $tmp/foo`

    if [ -n "$2" ]; then
      echo "$STR" | grep -q "$2"
      check "$1 type"
    fi

    if [ -n "$3" ]; then
      echo "$STR" | grep -q "$3"
      check "$1 linkage"
    fi
}

if [ `uname` = 'OpenBSD' ]; then
    test_obj '-fPIE -static' 'pie executable,' 'static-pie linked'
elif [ `uname` = 'Linux' ]; then
    test_obj '-fPIE -static-pie' 'pie executable,' 'static-pie linked'
fi

test_obj '-fPIE -pie' 'pie executable,' 'dynamically linked'

test_obj '-fno-PIE -no-pie' 'LSB executable,' 'dynamically linked'

test_obj '-fno-PIE -no-pie -static' 'LSB executable,' 'statically linked'

test_obj '-fPIC -shared' 'shared object,' 'dynamically linked'

test_obj '-r' 'relocatable'

echo OK

