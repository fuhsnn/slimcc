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

int ty_of_unqua(const int i1) {
  typeof_unqual(i1) i2;

  SASSERT(!__builtin_types_compatible_p(typeof(&i1), int *));
  SASSERT(__builtin_types_compatible_p(typeof(&i1), int const*));

  SASSERT(__builtin_types_compatible_p(typeof(&i2), int *));
  SASSERT(!__builtin_types_compatible_p(typeof(&i2), int const*));

  return i1;
}

int main() {
  ASSERT(true, fn());
  ASSERT(1, ty_of_unqua(1));

  printf("OK\n");
}
