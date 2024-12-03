REFCC=clang

TSTCC=$1

EPOCH=$(date +"%s")

MAX_JOBS=4

csmith -v

for ((seed = EPOCH; seed <= $((EPOCH + 605000)); seed++)); do
    if (( $(jobs -r -p | wc -l) >= MAX_JOBS )); then
        wait -n
    fi
  (
    if (( (seed % 5000) == 0 )); then
        echo "heartbeat:"$seed
    fi

    mkdir -p $seed

    csmith --no-packed-struct --seed $seed > $seed/main.c

    cd $seed

    $REFCC main.c -I /usr/include/csmith -lm -o ref.exe 2>/dev/null

    timeout 1 ./ref.exe > ref.txt
    if (( $? != 0 )); then
      cd ../ && rm -r $seed
      exit
    fi

    $TSTCC main.c -I /usr/include/csmith -lm -o tst.exe
    if (( $? != 0 )); then
      echo "tst cc failed at:"$seed
      touch ../has_error
      cd ../ && rm -r $seed
      exit
    fi

    timeout 10 ./tst.exe > tst.txt

    cmp -s ref.txt tst.txt
    if (( $? != 0 )); then
      echo "differ at:"$seed
      touch ../has_error
    fi

    cd ../ && rm -r $seed
  ) &
done

wait

if test -f ./has_error; then
  rm ./has_error
  exit 1
fi
