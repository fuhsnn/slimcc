testcc=$1

tmp=`mktemp -d /tmp/testcc-test-XXXXXX`
trap 'rm -rf $tmp' INT TERM HUP EXIT

check() {
    if [ $? -eq 0 ]; then
        echo "testing $1 ... passed"
    else
        echo "testing $1 ... failed"
        exit 1
    fi
}

mkdir -p $tmp/next1 $tmp/next2 $tmp/next3

echo '#include "next1/file1.h"' > $tmp/file.c
echo '#include_next "file1.h"'  > $tmp/next1/file1.h
echo 'foo'                      > $tmp/next2/file1.h
$testcc -I$tmp/next1 -I$tmp/next2 -E $tmp/file.c | grep -q foo
check '#include_next'

printf '"1"\n#include_next "c.h"\n"2"\n' > $tmp/next1/c.h
printf '"a"\n#include_next <c.h>\n"b"\n' > $tmp/next2/c.h
printf '"x"\n'                           > $tmp/next3/c.h
cat << EOF > $tmp/foo.c
#include <stdio.h>
int main() { puts(
#include INC
); }
EOF
$testcc -DINC=\"`realpath $tmp/next1/c.h`\" $tmp/foo.c -o $tmp/foo -I$tmp/next1 -I$tmp/next1 -I$tmp/next3 && $tmp/foo | grep -q '^111x222$' &&
$testcc -DINC=\"`realpath $tmp/next1/c.h`\" $tmp/foo.c -o $tmp/foo -I$tmp/next2 -I$tmp/next2 -I$tmp/next3 && $tmp/foo | grep -q '^11axb22$' &&
$testcc -DINC=\"`realpath $tmp/next1/c.h`\" $tmp/foo.c -o $tmp/foo -I$tmp/next1 -I$tmp/next2 -I$tmp/next3 && $tmp/foo | grep -q '^111axb222$' &&
$testcc -DINC=\"`realpath $tmp/next1/c.h`\" $tmp/foo.c -o $tmp/foo -I$tmp/next2 -I$tmp/next1 -I$tmp/next3 && $tmp/foo | grep -q '^11a1x2b22$'
check '#include_next'

$testcc -DINC=\"`realpath $tmp/next2/c.h`\" $tmp/foo.c -o $tmp/foo -I$tmp/next1 -I$tmp/next1 -I$tmp/next3 && $tmp/foo | grep -q '^a1x2b$' &&
$testcc -DINC=\"`realpath $tmp/next2/c.h`\" $tmp/foo.c -o $tmp/foo -I$tmp/next2 -I$tmp/next2 -I$tmp/next3 && $tmp/foo | grep -q '^aaxbb$' &&
$testcc -DINC=\"`realpath $tmp/next2/c.h`\" $tmp/foo.c -o $tmp/foo -I$tmp/next1 -I$tmp/next2 -I$tmp/next3 && $tmp/foo | grep -q '^a1axb2b$' &&
$testcc -DINC=\"`realpath $tmp/next2/c.h`\" $tmp/foo.c -o $tmp/foo -I$tmp/next2 -I$tmp/next1 -I$tmp/next3 && $tmp/foo | grep -q '^aa1x2bb$'
check '#include_next'

$testcc -DINC=\"next1/c.h\" $tmp/foo.c -o $tmp/foo -I$tmp/next1 -I$tmp/next1 -I$tmp/next3 && $tmp/foo | grep -q '^11x22$' &&
$testcc -DINC=\"next1/c.h\" $tmp/foo.c -o $tmp/foo -I$tmp/next2 -I$tmp/next2 -I$tmp/next3 && $tmp/foo | grep -q '^1axb2$' &&
$testcc -DINC=\"next1/c.h\" $tmp/foo.c -o $tmp/foo -I$tmp/next1 -I$tmp/next2 -I$tmp/next3 && $tmp/foo | grep -q '^11axb22$' &&
$testcc -DINC=\"next1/c.h\" $tmp/foo.c -o $tmp/foo -I$tmp/next2 -I$tmp/next1 -I$tmp/next3 && $tmp/foo | grep -q '^1a1x2b2$'
check '#include_next'

$testcc -DINC=\"next2/c.h\" $tmp/foo.c -o $tmp/foo -I$tmp/next1 -I$tmp/next1 -I$tmp/next3 && $tmp/foo | grep -q '^a1x2b$' &&
$testcc -DINC=\"next2/c.h\" $tmp/foo.c -o $tmp/foo -I$tmp/next2 -I$tmp/next2 -I$tmp/next3 && $tmp/foo | grep -q '^aaxbb$' &&
$testcc -DINC=\"next2/c.h\" $tmp/foo.c -o $tmp/foo -I$tmp/next1 -I$tmp/next2 -I$tmp/next3 && $tmp/foo | grep -q '^a1axb2b$' &&
$testcc -DINC=\"next2/c.h\" $tmp/foo.c -o $tmp/foo -I$tmp/next2 -I$tmp/next1 -I$tmp/next3 && $tmp/foo | grep -q '^aa1x2bb$'
check '#include_next'
