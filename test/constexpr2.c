#include "test.h"
#define SASSERT(x) _Static_assert(x); ASSERT(1, x)

int main(void) {

  SASSERT((_Bool)0.1f == 1);
  SASSERT((_Bool)2 == 1);


  printf("OK\n");
  return 0;
}
