set -u

skip_files=(
 # float conversion (implementation defined)
 c89/convert-float-unsigned.c
 undefined/assign-constant-float.c
 undefined/cast-float-overflow.c
 undefined/float-divide-zero.c

 # oversized array member, clang rejects
 limits/large-objects.c

 # trigraph
 c89/trigraph.c
 c89/line-continuation.c
)

fix_up() {
  # test5() GAS warning if the compiler pick +m
  sed -i 's|add $1, %0 \\n|addl $1, %0 \\n|g' asm/basic.c
}

match_skip() {
  local -n arr=$2
  for f in "${arr[@]}"; do
    if [ $f == $1 ];  then
      return 0
    fi
  done
  return 1
}

compile_test() {
  for src in */*.c; do
    if match_skip $src skip_files; then continue; fi

    local testarg
    if echo $src | grep ^linker -q; then
      continue
    elif echo $src | grep ^c89 -q; then
      testarg="-std=gnu89 $src"
    elif echo $src | grep ^c99 -q; then
      testarg="-std=gnu99 $src"
    elif echo $src | grep ^c11 -q; then
      testarg="-std=gnu11 $src"
    else
      testarg="-std=gnu99 $src"
    fi

    echo $src

    gcc $testarg -o ./_ref.exe 2>/dev/null
    if [ $? -ne 0 ]; then
      echo 'GCC compile failed'
      exit 1;
    fi

    $CC $testarg -o ./_tst.exe 2>/dev/null
    if [ $? -ne 0 ]; then
      echo '$CC compile failed'
      exit 1;
    fi

    ./_ref.exe 2>&1 >./_ref.txt
    local ext1=$?
    ./_tst.exe 2>&1 >./_tst.txt
    local ext2=$?

    if [ $ext1 -ne $ext2 ]; then
      echo "test exit code differ"
      exit 1
    fi
    if ! cmp ./_ref.txt ./_tst.txt; then
      echo "test output differ"
      exit 1
    fi
  done
}

cd test

fix_up
compile_test
