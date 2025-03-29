#define A

#ifndef A
#error
#elifndef B
#define B
#else
#error
#endif

#ifndef A
#error
#elifdef B
#else
#error
#endif


#if true
#define true_is_1
#endif

#ifndef true_is_1
#error
#endif

static_assert(true == (bool)123);

bool fn() {
  alignas(1024) int var;
  return ((long long)(&var) & 1023) == false;
}

#include "test.h"

int main() {
  ASSERT(true, fn());
  printf("OK\n");
}
