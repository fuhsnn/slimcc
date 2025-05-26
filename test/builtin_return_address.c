#include "test.h"

void *rtn_addr(int i) {
  switch (i) {
  case 2:
    return __builtin_return_address(2);
  case 1:
    ASSERT(1, rtn_addr(2) == __builtin_return_address(1));
    return __builtin_return_address(1);
  }
  ASSERT(1, __builtin_return_address(0) == rtn_addr(1));

  __auto_type builtin_return_address_test = __builtin_return_address(0);
  SASSERT(_Generic(&builtin_return_address_test, void **:1));
}

int main(void) {
  rtn_addr(0);

  printf("OK\n");
}
