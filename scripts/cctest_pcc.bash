set -u

skip_files=(
 # todo
 c99/func997.c

 # extra bracer in initializer for scalar member
 obsd/init003.c

 # math builtins
 c99/arith003.c
 jira/PCC-185.c
 jira/PCC-218.c

 # bit builtins
 pcclist/builtin001.c

 # other builtins
 pcclist/func001.c

 # __builtin_object_size
 jira/PCC-177.c
 pcclist/builtin002.c

 # trigraph
 c99/digraph001.c

 # __PRETTY_FUNCTION__
 jira/PCC-154.c

 # __VERSION__
 jira/PCC-210.c

 # _Complex
 c99/cmplx001.c
 c99/types002.c
 gcccompat/types001.c
 jira/PCC-57.c

 # _Decimal32
 gcccompat/types002.c

 # nested
 gcccompat/extension005.c

 # excess init list gcc does't like
 obsd/init999.c

 # old gcc designated initializer with colon
 jira/PCC-251.c

 # we allow non-static flexible struct
 c99/init998.c
)

fix_up() {
 sed -i 's|main()|int &|g' c99/basic008.c
 sed -i 's|main()|int &|g' c99/fpoint001.c
 sed -i 's|return 1;|return 0;|g' c99/func004.c
 sed -i 's|int printf();||g' misc/pic001.c
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

do_test() {
  for src in `find */*.c | sort -n`; do
    if match_skip $src skip_files; then continue; fi

    echo $src

    local testarg="-include stdio.h -include stdlib.h"
    if echo $src | grep ^c99 -q; then
      testarg="$testarg -std=gnu99 $src"
    elif echo $src | grep ^c23 -q; then
      testarg="$testarg -std=gnu23 $src"
    elif [ $src == 'misc/ucn001.c' ]; then
      testarg="$testarg -std=gnu99 $src"
    else
      testarg="$testarg -std=gnu89 $src"
    fi

    local gcc_rtn
    if [ $src == "c99/func996.c" ]; then
      gcc_rtn=1
    else
      gcc $testarg -c -o ./_ref.o 2>/dev/null
      gcc_rtn=$?
    fi

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
}

cd regress

fix_up
do_test
