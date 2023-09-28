#include "test.h"
#define SASSERT(x) _Static_assert(x); ASSERT(1, x)

#if !1u - 1 > 0
#error
#endif

int main(void) {

  SASSERT((_Bool)0.1f == 1);
  SASSERT((_Bool)2 == 1);
  SASSERT((_Bool)(0.0f + 0.1f) == 1);
  SASSERT((_Bool)(2 * 3) == 1);

  SASSERT( -1 < 0 );

  SASSERT( 3U << 31 >> 31 == 1);
  SASSERT( 1 << 31 >> 31 == -1);

  ASSERT(1, ({ int i = 2; char arr[ (i++,3) ]; i == sizeof arr; }) );

  printf("OK\n");
  return 0;
}
