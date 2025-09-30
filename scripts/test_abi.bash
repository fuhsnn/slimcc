set -eu

testcc="$1"
hostcc="$2"

compile_exec() {
 $testcc "$2" -Itest test/abi/"$1".c -c -o test/abi/"$1".o
 $hostcc "$2" -Itest test/host/"$1".c -c -o test/host/"$1".o
 $hostcc test/host/common.o test/abi/"$1".o test/host/"$1".o -o test/"$1"_abi.exe
 echo "testing $2"
 test/"$1"_abi.exe >/dev/null
}

compile_exec c89 '-std=c89'
compile_exec c11 '-std=c11'
compile_exec c23 '-std=c23'
