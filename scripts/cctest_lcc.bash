set -u

skip_files=(
 # invalid C89
 front.c
)

fix_up() {
 # depends on variable layout (implementation defined)
 sed -i 's|(goodtest,k|(badtest|g' cq.c
 # wrong formatter
 sed -i 's|%f %f %lf|%f %lf %Lf|g' cvt.c
 # va_arg(,char) is UB
 sed -i 's|(ap, char)|(ap, int)|g' stdarg.c
 sed -i 's|(ap, short)|(ap, int)|g' stdarg.c
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
  for src in *.c; do
    if match_skip $src skip_files; then continue; fi

    echo $src

    local input=`echo $src | sed -e 's|.c$|.0|g'`

    local testarg="$src -std=gnu89 -lm"

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

    cat $input | ./_ref.exe 2>&1 >./_ref.txt
    cat $input | ./_tst.exe 2>&1 >./_tst.txt

    if ! cmp ./_ref.txt ./_tst.txt; then
      echo "test output differ"
      exit 1
    fi
  done
}

cd tst

fix_up
compile_test
