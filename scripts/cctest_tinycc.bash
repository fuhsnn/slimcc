set -u

skip_files=(
 # unimplemented
 77_push_pop_macro.c
 95_bitfields.c # interection of pragma-pack with attribute packed/align
 95_bitfields_ms.c # [[gnu::ms_struct]]
 117_builtins.c
 136_atomic_gcc_style.c

 # invalid C
 34_array_assignment.c

 # windows specific
 99_fastcall.c
 113_btdll.c

 # tcc specific
 112_backtrace.c
 126_bound_global.c
)

dt_files=(
 60_errors_and_warnings.c
 96_nodata_wanted.c
 125_atomic_misc.c
 128_run_atexit.c
)

skip_defs=(
# todo
 illegal_unicode

# gcc atomic builtin
 atomic_error_2
 atomic_error_4
 atomic_op2
 atomic_store_struct

# gcc somehow allow initializing nested flexible array with string literal, clang rejects this
 var_array3

# dl depends on object layout in memory (UB)
 data_suppression_on

 # tcc specific
 reverse_funcargs
 normal_funcargs

# pragam comment(option,) (msvc-specific)
 switch_W1
 switch_W2
 switch_W3
 switch_W4
)

regen_expect() {
 gcc 2>/dev/null $1.c
 ./a.out > $1.expect
}

fix_up() {
  # .expect contain tcc specifics
  regen_expect 03_struct
  regen_expect 33_ternary_op
  regen_expect 70_floating_point_literals
  regen_expect 102_alignas

  # behavior difference, we prefer gcc's
  gcc 104_inline.c 104+_inline.c -std=gnu89
  ./a.out > 104_inline.expect

  # invalid according to gcc
  sed -i 's|^int alias_int |extern &|g' 120_alias.c
  sed -i 's|^int asm_int |extern &|g' 120_alias.c

#  # non-zero exit
#  sed -i 's|return i;|return i - 105;|g' 101_cleanup.c
#  sed -i 's|void main|int main|g' 137_funcall_struct_args.c

  # reduce thread use
  sed -i 's|NR_THREADS 16|NR_THREADS 4|g' 124_atomic_counter.c
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

main_test() {
  for src in `find *.c | sort -n`; do
    if match_skip $src skip_files; then continue; fi
    if match_skip $src dt_files; then continue; fi

    echo $src
    FLG="-std=gnu99"

    if [ $src == '102_alignas.c' ]; then
      FLG="-std=gnu89"
    fi

    if [ $src == '104+_inline.c' ]; then continue; fi
    if [ $src == '104_inline.c' ]; then
      FLG="-std=gnu89 104+_inline.c"
    fi

    if [ $src == '120+_alias.c' ]; then continue; fi
    if [ $src == '120_alias.c' ]; then
      FLG="-std=gnu99 120+_alias.c"
    fi

    $CC $src $FLG -lm -o ./_tst.exe 2>/dev/null
    if [ $? -ne 0 ]; then
      exit 1
    fi

    case $src in
      '31_args.c')
        ./_tst.exe > result arg1 arg2 arg3 arg4 arg5
      ;;
      '46_grep.c')
        ./_tst.exe > result '[^* ]*[:a:d: ]+\:\*-/: $$' 46_grep.c
      ;;
      *)
        ./_tst.exe > result
      ;;
    esac

    exp=`echo $src | sed -e 's|.c$|.expect|g'`

    diff -qZ ./result $exp
    if [ $? -ne 0 ]; then
      exit 1
    fi
  done
}

dt_test() {
  for src in ${dt_files[@]}; do
    local exp=`echo $src | sed -e 's|.c$|.expect|g'`

   for def in `cat $exp | sed -En 's/^\[test_(.*)\]$/\1/p'`; do
    if match_skip $def skip_defs; then continue; fi

    local testarg="$src -std=gnu89 -Dtest_$def"
    echo "$testarg"

    gcc $testarg -c -o ./_ref.o 2>/dev/null
    local gcc_rtn=$?

    $CC $testarg -c -o ./_tst.o 2>/dev/null
    local tst_rtn=$?

    if [ $((! $gcc_rtn)) -ne $((! $tst_rtn)) ]; then
      echo "compiler exit code differ"
      exit 1
    fi
    if [ $gcc_rtn -ne 0 ]; then continue; fi

    gcc ./_ref.o -lm -o ./_ref.exe 2>/dev/null
    gcc_rtn=$?

    $CC ./_tst.o -lm -o ./_tst.exe 2>/dev/null
    tst_rtn=$?

    if [ $((! $gcc_rtn)) -ne $((! $tst_rtn)) ]; then
      echo "linker exit code differ"
      exit 1
    fi
    if [ $gcc_rtn -ne 0 ]; then continue; fi

    ./_ref.exe 2>&1 >./_ref.txt
    ./_tst.exe 2>&1 >./_tst.txt

    if ! cmp ./_ref.txt ./_tst.txt; then
      echo "test output differ"
      exit 1
    fi
   done
  done
}

cd tests/tests2

fix_up
main_test
dt_test
