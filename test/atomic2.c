#include "test.h"
#include <stdatomic.h>

int ptr_arith(void) {
    int32_t *_Atomic p = NULL;
    int32_t *p2 = atomic_fetch_add(&p, 1);
#ifdef NOTGCC
    ASSERT(4, (long)p);
#endif
    ASSERT(0, (long)p2);
  return 1;
}

int f32(void) {
  _Atomic float f = 5.0f;

  ASSERT(1, 8.0f == (f += 3.0f));
#ifdef NOTGCC
  ASSERT(1, 8.0f == atomic_fetch_sub(&f, 2.0f));
  ASSERT(1, 6.0f == f);
#endif
  return 1;
}

int f64(void) {
  _Atomic double d = 10.0;

  ASSERT(1, 7.0 == (d -= 3.0));
#ifdef NOTGCC
  ASSERT(1, 7.0 == atomic_fetch_add(&d, 2.0));
  ASSERT(1, 9.0 == d);
#endif
  return 1;
}

int main(void) {
  ASSERT(1, ptr_arith());
  ASSERT(1, f32());
  ASSERT(1, f64());

  printf("OK\n");
}
