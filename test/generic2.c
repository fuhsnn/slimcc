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

  ASSERT(1, _Generic(argc ? 0 : "a", char*:1 ) );
  ASSERT(1, _Generic(argc ? "a" : 0, char*:1 ) );

  ASSERT(1, _Generic(argc ? (void*)0 : "a", char*:1 ) );
  ASSERT(1, _Generic(argc ? "a" : (void*)0, char*:1 ) );

  ASSERT(1, _Generic(argc ? "a" : (void*)(void*)0, void*:1 ) );

  ASSERT(1, ({ void *p; _Generic(argc ? p : "a", void*:1, char*:2); }) );
  ASSERT(1, ({ void *p; _Generic(argc ? "a" : p, void*:1, char*:2); }) );

  ASSERT(1, ({ double *p; char *q; _Generic(argc ? p : q, void*:1 ); }) );
  ASSERT(1, ({ double *p; char *q; _Generic(argc ? q : p, void*:1 ); }) );

  ASSERT(1, ({ int vla[argc]; _Generic(vla, int*:1 ); }) );

  printf("OK\n");
}

