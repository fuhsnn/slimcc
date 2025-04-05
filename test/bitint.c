#include "test.h"

SASSERT(sizeof(_BitInt(7)) == 1);
SASSERT(sizeof(_BitInt(15)) == 2);
SASSERT(sizeof(_BitInt(31)) == 4);
SASSERT(sizeof(_BitInt(63)) == 8);

SASSERT(_Alignof(_BitInt(7)) == 1);
SASSERT(_Alignof(_BitInt(15)) == 2);
SASSERT(_Alignof(_BitInt(31)) == 4);
SASSERT(_Alignof(_BitInt(63)) == 8);

SASSERT(sizeof(_BitInt(7)[17]) == 17);
SASSERT(sizeof(_BitInt(15)[17]) == 34);
SASSERT(sizeof(_BitInt(31)[17]) == 68);
SASSERT(sizeof(_BitInt(63)[17]) == 136);

int main() {
  printf("OK\n");
}
