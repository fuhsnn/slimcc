set -u

skip_files=(
 # observing enum value type before the enum type is completed
 enum-large-value.c

 # unimplemented
 builtin-inff.c
 builtin-nanf.c

 # VM in typeof
 typeof-vm.c
)

fix_up() {
  for src in varargs*.c; do
    sed -i 's|__builtin_va_start(ap)|__builtin_c23_va_start(ap)|g' $src
  done
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
  for src in `ls *.c | grep -v '^preprocess-' | grep -v aarch64 | grep -v riscv64` ; do
    if match_skip $src skip_files; then continue; fi

    echo $src
    $CC $src -std=c23 -c -o ./_tst.o
    if [ $? -ne 0 ]; then exit 1; fi

    $CC ./_tst.o -o ./_tst.exe 2>/dev/null
    if [ $? -ne 0 ]; then continue; fi

    ./_tst.exe
    if [ $? -ne 0 ]; then exit 1; fi
  done
}

cd test

fix_up
compile_test
