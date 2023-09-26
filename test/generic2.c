#include "test.h"
int main(int argc, char**argv) {

  ASSERT(1, ({ char c; _Generic(c << 1, int:1 ); }) );
  ASSERT(1, ({ short s; _Generic(s >> 1, int:1 ); }) );
  ASSERT(1, ({ struct { unsigned int c : 17; } s; _Generic(~s.c, int:1 ); }) );
  ASSERT(1, ({ struct { unsigned long long c : 17; } s; _Generic(~s.c, int:1 ); }) );

  ASSERT(1, ({ char c; _Generic(-c, int:1 ); }) );
  ASSERT(1, ({ short s; _Generic(+s, int:1 ); }) );

  ASSERT(1, _Generic((long){0}, long:1, long long:0) );
  ASSERT(1, _Generic((long long){0}, long long:1, long:0) );

  ASSERT(1, _Generic(1L, long:1, long long:0) );
  ASSERT(1, _Generic(1LL, long long:1, long:0) );

  ASSERT(1, _Generic(1UL, unsigned long:1, unsigned long long:0) );
  ASSERT(1, _Generic(1lu, unsigned long:1, unsigned long long:0) );

  ASSERT(1, _Generic(1ULL, unsigned long long:1, unsigned long:0) );
  ASSERT(1, _Generic(1llu, unsigned long long:1, unsigned long:0) );

  ASSERT(1, ({ unsigned long a; long long b; _Generic(a + b, unsigned long long:1, unsigned long:0); }) );

  printf("OK\n");
}

