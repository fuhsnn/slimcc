set -u

err_skips=(
 # '##' at start/end of never used macro
 error/0035-cpp.c

 # infinite #include recursion
 error/0003-junkinclude.c

 # k&r old style function
 error/0019-kr_names.c
 error/0030-krtypes.c
 error/0031-krtypes.c
 error/0032-krtypes.c

 # gcc allow
 error/0004-macroredef.c
 error/0016-arrayinitsize.c
 error/0022-cpp-if.c
 error/0025-bad-init.c
 error/0027-constoverflow.c
 error/0028-noconstinit.c
 error/0039-struct.c

 # old gcc allow
 error/0037-pointer.c
)

exec_skips=(
 # initializer override
 execute/0166-desig.c

 # (i % 0) shouldn't be folded
 execute/0140-int_fold.c
)

fix_up() {
 sed -i 's|dummy;|dummy{int i;};|g' execute/0180-incomplete.c
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

err_test() {
  for src in error/*.c; do
    if match_skip $src err_skips; then continue; fi
    echo $src
    $CC $src -S -o/dev/null 2>./warn_log.txt
    if [ $? -eq 0 ]; then exit 1; fi
    if grep 'internal error' ./warn_log.txt; then exit 1; fi
    if grep AddressSanitizer ./warn_log.txt; then exit 1; fi
  done
}

exec_test() {
  for src in execute/0*.c; do
    if match_skip $src exec_skips; then continue; fi

    local ARG
    if [ $src == 'execute/0026-implicitret.c' ] ||
       [ $src == 'execute/0121-localinit.c' ] ||
       [ $src == 'execute/0128-kr_names.c' ] ||
       [ $src == 'execute/0143-int_const.c' ] ||
       [ $src == 'execute/0156-duff2.c' ]; then
      ARG='-std=c89'
    elif [ $src == 'execute/0193-incomplete.c' ]; then
      ARG='-std=c99 execute/test.c'
    else
      ARG='-std=c99'
    fi

    echo $src
    $CC $src $ARG -Iexecute/sysinclude -D__SCC__=1 -o ./_tst.exe
    if [ $? -ne 0 ]; then
      echo 'compile failed'
      exit 1;
    fi

    ./_tst.exe >/dev/null
    if [ $? -ne 0 ]; then
      echo 'exec failed'
      exit 1
    fi
  done
}

cd tests/cc/

fix_up
err_test
exec_test
