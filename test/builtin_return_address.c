#include "test.h"

void *frame_addr(int i) {
  switch (i) {
  case 2:
    return __builtin_frame_address(2);
  case 1:
    ASSERT(1, frame_addr(2) == __builtin_frame_address(1));
    return __builtin_frame_address(1);
  }
  ASSERT(1, __builtin_frame_address(0) == frame_addr(1));

  __auto_type builtin_frame_address_test = __builtin_frame_address(0);
  SASSERT(_Generic(&builtin_frame_address_test, void **:1));
}

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
  frame_addr(0);
  rtn_addr(0);

  printf("OK\n");
}
