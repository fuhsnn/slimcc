#include "test.h"

int main() {
  { int const x; }
  { const int x; }
  { const int const const x; }
  ASSERT(5, ({ const int x = 5; x; }));
  ASSERT(8, ({ int const x = 8; int *const y=&x; *y; }));
  ASSERT(6, ({ const int x = 6; *(int const * const)&x; }));

  printf("OK\n");
  return 0;
}
