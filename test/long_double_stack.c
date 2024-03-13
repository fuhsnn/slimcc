#include "test.h"

long double clobber(void) {
  asm("fninit\n");

  return 2.0L;
}

long double add(long double a) {
  return  a + (a + (a + (a + (a + (a + (a + (a + a)))))));
}

long double sub(long double a, long double b) {
  return a - (b - (b - (b - (b - (b - (b - (b - a)))))));
}

int main(void) {
  ASSERT(1, 10.0L == (3.0L + clobber() + 5.0L));
  ASSERT(1, 71.0L == (3.0L + add(7.0L) + 5.0L));
  ASSERT(1, 200.0L == (3.0L + sub(97.0L, 2.0L) + 5.0L));
  ASSERT(1, 191.9L < sub(97.0L, 2.0L));

  ASSERT(1, 9.0L == (3.0L + (1.0L, 1.0L, 1.0L, 1.0L, 1.0L, 1.0L, 1.0L, 1.0L) + 5.0L));

  long double v = -1.0L;
  ASSERT(1, 7.0L == (3.0L + ({v; v; v; v; v; v; v; v;}) + 5.0L));

  long double w,x,y,z;
  w = x = y = z = v;
  ASSERT(1, -1.0L == w);
  ASSERT(1, -1.0L == x);
  ASSERT(1, -1.0L == y);
  ASSERT(1, -1.0L == z);

  printf("OK\n");
}
