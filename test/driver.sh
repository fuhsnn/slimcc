#!/bin/bash
testcc=$1
refcc=$2

tmp=`mktemp -d /tmp/testcc-test-XXXXXX`
trap 'rm -rf $tmp' INT TERM HUP EXIT
echo > $tmp/empty.c

check() {
    if [ $? -eq 0 ]; then
        echo "testing $1 ... passed"
    else
        echo "testing $1 ... failed"
        exit 1
    fi
}

# -o
rm -f $tmp/out
$testcc -c -o $tmp/out $tmp/empty.c
[ -f $tmp/out ]
check -o

# -S
echo 'int main() {}' | $testcc -S -o- -xc - | grep -q '"main":'
check -S

# Default output file
rm -f $tmp/out.o $tmp/out.s
echo 'int main() {}' > $tmp/out.c
(cd $tmp; $OLDPWD/$testcc -c out.c)
[ -f $tmp/out.o ]
check 'default output file'

(cd $tmp; $OLDPWD/$testcc -c -S out.c)
[ -f $tmp/out.s ]
check 'default output file'

# Multiple input files
rm -f $tmp/foo.o $tmp/bar.o
echo 'int x;' > $tmp/foo.c
echo 'int y;' > $tmp/bar.c
(cd $tmp; $OLDPWD/$testcc -c $tmp/foo.c $tmp/bar.c)
[ -f $tmp/foo.o ] && [ -f $tmp/bar.o ]
check 'multiple input files'

rm -f $tmp/foo.s $tmp/bar.s
echo 'int x;' > $tmp/foo.c
echo 'int y;' > $tmp/bar.c
(cd $tmp; $OLDPWD/$testcc -c -S $tmp/foo.c $tmp/bar.c)
[ -f $tmp/foo.s ] && [ -f $tmp/bar.s ]
check 'multiple input files'

# Run linker
rm -f $tmp/foo
echo 'int main() { return 0; }' | $testcc -o $tmp/foo -xc -xc -
$tmp/foo
check linker

rm -f $tmp/foo
echo 'int bar(); int main() { return bar(); }' > $tmp/foo.c
echo 'int bar() { return 42; }' > $tmp/bar.c
$testcc -o $tmp/foo $tmp/foo.c $tmp/bar.c
$tmp/foo
[ "$?" = 42 ]
check linker

# a.out
rm -f $tmp/a.out
echo 'int main() {}' > $tmp/foo.c
(cd $tmp; $OLDPWD/$testcc foo.c)
[ -f $tmp/a.out ]
check a.out

# -E
echo foo > $tmp/out
echo "#include \"$tmp/out\"" | $testcc -E -xc - | grep -q foo
check -E

echo foo > $tmp/out1
echo "#include \"$tmp/out1\"" | $testcc -E -o $tmp/out2 -xc -
cat $tmp/out2 | grep -q foo
check '-E and -o'

# -I
mkdir $tmp/dir
echo foo > $tmp/dir/i-option-test
echo "#include \"i-option-test\"" | $testcc -I$tmp/dir -E -xc - | grep -q foo
check -I

echo "#include \"i-option-test\"" | $testcc -I $tmp/dir -E -xc - | grep -q foo
check -I

# -D
echo foo | $testcc -Dfoo -E -xc - | grep -q 1
check -D

# -D
echo foo | $testcc -Dfoo=bar -E -xc - | grep -q bar
check -D

# -U
echo foo | $testcc -Dfoo=bar -Ufoo -E -xc - | grep -q foo
check -U

# ignored options
$testcc -c -O -Wall -g -std=c11 -ffreestanding -fno-builtin \
         -fno-omit-frame-pointer -fno-stack-protector -fno-strict-aliasing \
         -m64 -mno-red-zone -w -o /dev/null $tmp/empty.c
check 'ignored options'

# BOM marker
printf '\xef\xbb\xbfxyz\n' | $testcc -E -o- -xc - | grep -q '^xyz'
check 'BOM marker'

# Inline functions
echo 'inline void foo() {}' > $tmp/inline1.c
echo 'inline void foo() {}' > $tmp/inline2.c
echo 'int main() { return 0; }' > $tmp/inline3.c
$testcc -o /dev/null $tmp/inline1.c $tmp/inline2.c $tmp/inline3.c
check inline

echo 'extern inline void foo() {}' > $tmp/inline1.c
echo 'int foo(); int main() { foo(); }' > $tmp/inline2.c
$testcc -o /dev/null $tmp/inline1.c $tmp/inline2.c
check inline

echo 'static inline void f1() {}' | $testcc -o- -S -xc - | grep -v -q \"f1\":
check inline

echo 'static inline void f1() {} void foo() { f1(); }' | $testcc -o- -S -xc - | grep -q \"f1\":
check inline

echo 'static inline void f1() {} static inline void f2() { f1(); } void foo() { f1(); }' | $testcc -o- -S -xc - | grep -q \"f1\":
check inline

echo 'static inline void f1() {} static inline void f2() { f1(); } void foo() { f1(); }' | $testcc -o- -S -xc - | grep -v -q \"f2\":
check inline

echo 'static inline void f1() {} static inline void f2() { f1(); } void foo() { f2(); }' | $testcc -o- -S -xc - | grep -q \"f1\":
check inline

echo 'static inline void f1() {} static inline void f2() { f1(); } void foo() { f2(); }' | $testcc -o- -S -xc - | grep -q \"f2\":
check inline

echo 'static inline void f2(); static inline void f1() { f2(); } static inline void f2() { f1(); } void foo() {}' | $testcc -o- -S -xc - | grep -v -q \"f1\":
check inline

echo 'static inline void f2(); static inline void f1() { f2(); } static inline void f2() { f1(); } void foo() {}' | $testcc -o- -S -xc - | grep -v -q \"f2\":
check inline

echo 'static inline void f2(); static inline void f1() { f2(); } static inline void f2() { f1(); } void foo() { f1(); }' | $testcc -o- -S -xc - | grep -q \"f1\":
check inline

echo 'static inline void f2(); static inline void f1() { f2(); } static inline void f2() { f1(); } void foo() { f1(); }' | $testcc -o- -S -xc - | grep -q \"f2\":
check inline

echo 'static inline void f2(); static inline void f1() { f2(); } static inline void f2() { f1(); } void foo() { f2(); }' | $testcc -o- -S -xc - | grep -q \"f1\":
check inline

echo 'static inline void f2(); static inline void f1() { f2(); } static inline void f2() { f1(); } void foo() { f2(); }' | $testcc -o- -S -xc - | grep -q \"f2\":
check inline

# -idirafter
mkdir -p $tmp/dir1 $tmp/dir2
echo foo > $tmp/dir1/idirafter
echo bar > $tmp/dir2/idirafter
echo "#include \"idirafter\"" | $testcc -I$tmp/dir1 -I$tmp/dir2 -E -xc - | grep -q foo
check -idirafter
echo "#include \"idirafter\"" | $testcc -idirafter $tmp/dir1 -I$tmp/dir2 -E -xc - | grep -q bar
check -idirafter

# -fcommon
echo 'int foo;' | $testcc -S -o- -xc - | grep -q '\.comm "foo"'
check '-fcommon (default)'

echo 'int foo;' | $testcc -fcommon -S -o- -xc - | grep -q '\.comm "foo"'
check '-fcommon'

# -fno-common
echo 'int foo;' | $testcc -fno-common -S -o- -xc - | grep -q '^"foo":'
check '-fno-common'

# -funsigned-char
rm -f $tmp/out
echo 'int main(void){ return (char)-1 != (unsigned char)-1; }' | $testcc -funsigned-char -xc - -o $tmp/out
$tmp/out
check '-funsigned-char'

# -ffunction-sections
echo 'void f1() {}' | $testcc -xc - -S -o- -ffunction-sections | grep -q '.text."f1",'
check '-ffunction-sections'

# -fdata-sections
echo 'int var = 1;' | $testcc -xc - -S -o- -fdata-sections | grep -q '.data."var",'
check '-fdata-sections'
echo '_Thread_local int var = 1;' | $testcc -xc - -S -o- -fdata-sections | grep -q '.tdata."var",'
check '-fdata-sections'
echo 'void fn(void){static int var;}' | $testcc -xc - -S -o- -fdata-sections | grep -q '.bss.'
check '-fdata-sections'
echo '_Thread_local int var;' | $testcc -xc - -S -o- -fdata-sections | grep -q '.tbss."var",'
check '-fdata-sections'

# -include
echo foo > $tmp/out.h
echo bar | $testcc -include $tmp/out.h -E -o- -xc - | grep -q -z 'foo.*bar'
check -include
echo NULL | $testcc -Iinclude -include stdio.h -E -o- -xc - | grep -q 0
check -include

# -x
echo 'int x;' | $testcc -c -xc -o $tmp/foo.o -
check -xc
echo 'x:' | $testcc -c -x assembler -o $tmp/foo.o -
check '-x assembler'

echo 'int x;' > $tmp/foo.c
$testcc -c -x assembler -x none -o $tmp/foo.o $tmp/foo.c
check '-x none'

# -E
echo foo | $testcc -E - | grep -q foo
check -E

# .a file
echo 'void foo() {}' | $testcc -c -xc -o $tmp/foo.o -
echo 'void bar() {}' | $testcc -c -xc -o $tmp/bar.o -
ar rcs $tmp/foo.a $tmp/foo.o $tmp/bar.o
echo 'void foo(); void bar(); int main() { foo(); bar(); }' > $tmp/main.c
$testcc -o $tmp/foo $tmp/main.c $tmp/foo.a
check '.a'

# .so file
echo 'void foo() {}' | $refcc -fPIC -c -xc -o $tmp/foo.o -
echo 'void bar() {}' | $refcc -fPIC -c -xc -o $tmp/bar.o -
$refcc -shared -o $tmp/foo.so $tmp/foo.o $tmp/bar.o
echo 'void foo(); void bar(); int main() { foo(); bar(); }' > $tmp/main.c
$testcc -o $tmp/foo $tmp/main.c $tmp/foo.so
check '.so'

$testcc -hashmap-test
check 'hashmap'

# -M
echo '#include "out2.h"' > $tmp/out.c
echo '#include "out3.h"' >> $tmp/out.c
touch $tmp/out2.h $tmp/out3.h
$testcc -M -I$tmp $tmp/out.c | grep -q -z '^out.o: .*/out\.c .*/out2\.h .*/out3\.h'
check -M

# -MF
$testcc -MF $tmp/mf -M -I$tmp $tmp/out.c
grep -q -z '^out.o: .*/out\.c .*/out2\.h .*/out3\.h' $tmp/mf
check -MF

# -MP
$testcc -MF $tmp/mp -MP -M -I$tmp $tmp/out.c
grep -q '^.*/out2.h:' $tmp/mp
check -MP
grep -q '^.*/out3.h:' $tmp/mp
check -MP

# -MT
$testcc -MT foo -M -I$tmp $tmp/out.c | grep -q '^foo:'
check -MT
$testcc -MT foo -MT bar -M -I$tmp $tmp/out.c | grep -q '^foo bar:'
check -MT

# -MD
echo '#include "out2.h"' > $tmp/md2.c
echo '#include "out3.h"' > $tmp/md3.c
(cd $tmp; $OLDPWD/$testcc -c -MD -I. md2.c md3.c)
grep -q -z '^md2.o:.* md2\.c .* ./out2\.h' $tmp/md2.d
check -MD
grep -q -z '^md3.o:.* md3\.c .* ./out3\.h' $tmp/md3.d
check -MD

$testcc -c -MD -MF $tmp/md-mf.d -I. $tmp/md2.c
grep -q -z '^md2.o:.*md2\.c .*/out2\.h' $tmp/md-mf.d
check -MD

echo 'extern int bar; int foo() { return bar; }' | $testcc -fPIC -xc -c -o $tmp/foo.o -
$refcc -shared -o $tmp/foo.so $tmp/foo.o
echo 'int foo(); int bar=3; int main() { foo(); }' > $tmp/main.c
$testcc -o $tmp/foo $tmp/main.c $tmp/foo.so
check -fPIC

# #include_next
mkdir -p $tmp/next1 $tmp/next2 $tmp/next3
echo '#include "file1.h"' > $tmp/file.c
echo '#include_next "file1.h"' > $tmp/next1/file1.h
echo '#include_next "file2.h"' > $tmp/next2/file1.h
echo 'foo' > $tmp/next3/file2.h
$testcc -I$tmp/next1 -I$tmp/next2 -I$tmp/next3 -E $tmp/file.c | grep -q foo
check '#include_next'

# -static
echo 'extern int bar; int foo() { return bar; }' > $tmp/foo.c
echo 'int foo(); int bar=3; int main() { foo(); }' > $tmp/bar.c
$testcc -static -o $tmp/foo $tmp/foo.c $tmp/bar.c
check -static
file $tmp/foo | grep -q 'statically linked'
check -static

# -shared
echo 'extern int bar; int foo() { return bar; }' > $tmp/foo.c
echo 'int foo(); int bar=3; int main() { foo(); }' > $tmp/bar.c
$testcc -fPIC -shared -o $tmp/foo.so $tmp/foo.c $tmp/bar.c
check -shared

# -L
echo 'extern int bar; int foo() { return bar; }' > $tmp/foo.c
$testcc -fPIC -shared -o $tmp/libfoobar.so $tmp/foo.c
echo 'int foo(); int bar=3; int main() { foo(); }' > $tmp/bar.c
$testcc -o $tmp/foo $tmp/bar.c -L$tmp -lfoobar
check -L

# -Wl,
echo 'int foo() {}' | $testcc -c -o $tmp/foo.o -xc -
echo 'int foo() {}' | $testcc -c -o $tmp/bar.o -xc -
echo 'int main() {}' | $testcc -c -o $tmp/baz.o -xc -
$testcc -Wl,-z,muldefs,--gc-sections -o $tmp/foo $tmp/foo.o $tmp/bar.o $tmp/baz.o
check -Wl,

# -Xlinker
echo 'int foo() {}' | $testcc -c -o $tmp/foo.o -xc -
echo 'int foo() {}' | $testcc -c -o $tmp/bar.o -xc -
echo 'int main() {}' | $testcc -c -o $tmp/baz.o -xc -
$testcc -Xlinker -z -Xlinker muldefs -Xlinker --gc-sections -o $tmp/foo $tmp/foo.o $tmp/bar.o $tmp/baz.o
check -Xlinker

echo OK
