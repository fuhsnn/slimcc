set -u

TSTCC=$1

SRC=$2

OUT=$3

FLAGS='-Itest/ -std=gnu23'

for p in `sed -n '/\/\/SREJ /=' $SRC`; do
  sed -e $p's|\/\/SREJ ||g' $SRC | $TSTCC -xc - $FLAGS -S -o/dev/null 2>/dev/null
  if [ $? -ne 1 ]; then
    echo "$SRC:$p expected compilation error"
    exit 1
  fi
done

$TSTCC $SRC $FLAGS -pthread test/host/common.o -o $OUT
