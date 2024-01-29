#include "test.h"
#define DASSERT(x) _Static_assert(x); ASSERT(1, x)

#if !1u - 1 > 0
#error
#endif

int main(void) {

  DASSERT((_Bool)0.1f == 1);
  DASSERT((_Bool)2 == 1);
  DASSERT((_Bool)(0.0f + 0.1f) == 1);
  DASSERT((_Bool)(2 * 3) == 1);

  DASSERT( -1 < 0 );

  DASSERT( 3U << 31 >> 31 == 1);
  DASSERT( 1 << 31 >> 31 == -1);

  DASSERT(0.2 > 0.1);
  DASSERT(0.1 < 0.2);
  DASSERT(0.2 != 0.1);
  DASSERT(!(0.2 <= 0.1));
  DASSERT(!(0.1 >= 0.2));
  DASSERT(!(0.1 == 0.2));
  DASSERT(!!0.1);

  DASSERT(13835058055282163712.0  == (double)13835058055282163712ULL);
  DASSERT(13835058055282163712.0f == (float) 13835058055282163712ULL);
  DASSERT(13835058055282163712ULL == (unsigned long long) 13835058055282163712.0f);
  DASSERT(13835058055282163712ULL == (unsigned long long) 13835058055282163712.0);

  ASSERT(1, 13835058055282163711ULL  == (unsigned long long) 13835058055282163711.0L);
  ASSERT(1, 13835058055282163711.0L  == (long double) 13835058055282163711ULL);

  DASSERT( 16777217.0 != (float)16777217.0 );
  DASSERT( 9007199254740993.0L != (double)9007199254740993.0L || sizeof(double) == sizeof(long double) );

  ASSERT(1, ({ int i = 2; char arr[ (i++,3) ]; i == sizeof arr; }) );

  printf("OK\n");
  return 0;
}
