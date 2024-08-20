#include "test.h"

void paramty(int a[volatile], int sz, int b[const restrict static sz]) {
  ASSERT(1, _Generic(a, int *: 1, int volatile*: 0));
  ASSERT(1, _Generic(a, int volatile*: 0, int *: 1));

  ASSERT(1, _Generic(&a, int **: 0, int *volatile *: 1));
  ASSERT(1, _Generic(&a, int *volatile *: 1, int **: 0));

  ASSERT(1, _Generic(&b, int **: 0, int *restrict const *: 1));
  ASSERT(1, _Generic(&b, int *restrict const *: 1, int **: 0));
}

void param_arrptr_ty(int sz, int(*a)[13], int(*b)[]) {
  ASSERT(1, _Generic(a, default:0, int(*)[13]:1, int(*)[17]:2));
  ASSERT(0, _Generic(a, default:0, int(*)[13][17]:1));

  ASSERT(1, _Generic(b, default:0, typeof(a):1));
  ASSERT(1, _Generic(a, default:0, typeof(b):1));

  ASSERT(1, _Generic(b, default:0, int(*)[17]:1));
  ASSERT(0, _Generic(b, default:0, int(*)[13][17]:1));

  int vla1[sz];
  int vla2[sz][13];
  ASSERT(1, _Generic(&vla1, default :0, typeof(a):1));
  ASSERT(1, _Generic(&vla1, default :0, typeof(b):1));
  ASSERT(0, _Generic(&vla2, default :0, typeof(a):1));
  ASSERT(1, _Generic(&vla2[1], default :0, typeof(a):1));
}


void param_vlaptr_ty(int sz, int (*a)[sz]) {
  ASSERT(0, _Generic(a, default:0, int *:1));
  ASSERT(1, _Generic(a, default:0, int(*)[13]:1));
  ASSERT(0, _Generic(a, default:0, int(*)[13][17]:1));
}

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
  ASSERT(1, _Generic(argc ? (void*)(void*)0 : "a", void*:1 ) );

  ASSERT(1, ({ void *p; _Generic(argc ? p : "a", void*:1, char*:2); }) );
  ASSERT(1, ({ void *p; _Generic(argc ? "a" : p, void*:1, char*:2); }) );

  ASSERT(1, ({ double *p; char *q; _Generic(argc ? p : q, void*:1 ); }) );
  ASSERT(1, ({ double *p; char *q; _Generic(argc ? q : p, void*:1 ); }) );

  ASSERT(1, _Generic(**argv, unsigned char:0, char:1, signed char:0) );
  ASSERT(1, _Generic("a", unsigned char*:0, char*:1, signed char*:0) );

  ASSERT(1, ({ int vla[argc]; _Generic(vla, int*:1 ); }) );

  ASSERT(1, ({ const int *p; _Generic(p, int const *:1, int *:0);}));
  ASSERT(1, ({ volatile int *p; _Generic(p, int *:0, int volatile *:1);}));
  ASSERT(1, ({ _Atomic int *p; _Generic(p, int *:0, int _Atomic *:1);}));
  ASSERT(1, ({ int *_Atomic p; _Generic(p, int *:1);}));
  ASSERT(1, ({ int *_Atomic *p; _Generic(p, int *_Atomic *:1, int**:0);}));
  ASSERT(1, ({ int *_Atomic *p; _Generic(p, int**:0, int *_Atomic *:1);}));
  ASSERT(1, ({ int *restrict p; _Generic(p, int *:1);}));
  ASSERT(1, ({ int *restrict *p; _Generic(p, int *restrict *:1, int **:0);}));
  ASSERT(1, ({ int *restrict *p; _Generic(p, int **:0, int *restrict *:1);}));

#ifdef NOTCLANG
  ASSERT(1, ({ const int (*fn)(void); _Generic(fn, int(*)(void): 1);}));
#endif
  ASSERT(1, ({ const int(*fn)(void); typeof(fn()) i; _Generic(&i, int *:1);}));
  ASSERT(1, ({ void (*fn)(const int); _Generic(fn, void(*)(int): 1);}));

  ASSERT(1, ({ const int i; _Generic(i, int :1, const int: 2);}));

  paramty(0,0,0);
  param_arrptr_ty(0,0,0);
  param_vlaptr_ty(0,0);


  printf("OK\n");
}

