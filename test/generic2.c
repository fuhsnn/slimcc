#include "test.h"

void paramty(int a[volatile], int sz, int b[static const restrict sz]) {
  SASSERT(1 == _Generic(a, int *: 1, int volatile*: 0));
  SASSERT(1 == _Generic(a, int volatile*: 0, int *: 1));

  SASSERT(1 == _Generic(&a, int **: 0, int *volatile *: 1));
  SASSERT(1 == _Generic(&a, int *volatile *: 1, int **: 0));

  SASSERT(1 == _Generic(&b, int **: 0, int *restrict const *: 1));
  SASSERT(1 == _Generic(&b, int *restrict const *: 1, int **: 0));
}

void param_arrptr_ty(int sz, int(*a)[13], int(*b)[]) {
  SASSERT(1 == _Generic(a, default:0, int(*)[13]:1, int(*)[17]:2));
  SASSERT(0 == _Generic(a, default:0, int(*)[13][17]:1));

  SASSERT(1 == _Generic(b, default:0, typeof(a):1));
  SASSERT(1 == _Generic(a, default:0, typeof(b):1));

  SASSERT(1 == _Generic(b, default:0, int(*)[17]:1));
  SASSERT(0 == _Generic(b, default:0, int(*)[13][17]:1));

  int vla1[sz];
  int vla2[sz][13];
  SASSERT(1 == _Generic(&vla1, default :0, typeof(a):1));
  SASSERT(1 == _Generic(&vla1, default :0, typeof(b):1));
  SASSERT(0 == _Generic(&vla2, default :0, typeof(a):1));
  SASSERT(1 == _Generic(&vla2[1], default :0, typeof(a):1));
}

void param_vlaptr_ty(int sz, int (*a)[sz]) {
  SASSERT(0 == _Generic(a, default:0, int *:1));
  SASSERT(1 == _Generic(a, default:0, int(*)[13]:1));
  SASSERT(0 == _Generic(a, default:0, int(*)[13][17]:1));
}

void param_array_decay_with_qualifier(
  typeof(int[1]) t1,
  typeof(int[1]) const t2,
  typeof(int[1][1]) t3,
  typeof(int[1][1]) const t4,
  typeof(int[1]) t5[],
  typeof(int[1]) t6[volatile],
  typeof(int[1]) volatile t7[],
  typeof(int[1]) volatile t8[const],
  typeof(const int[1]) volatile t9)
{
  SASSERT(_Generic(       t1 , int *: 1));
  SASSERT(_Generic(       t2 , int const *: 1));
  SASSERT(_Generic(       t3 , typeof(int[1]) *: 1));
  SASSERT(_Generic(       t4 , typeof(int[1]) const *: 1));
  SASSERT(_Generic(       t5 , typeof(int[1]) *: 1));
  SASSERT(_Generic(       t6 , typeof(int[1]) *: 1));
  SASSERT(_Generic(typeof(t6), typeof(int[1]) * volatile: 1));
  SASSERT(_Generic(       t7 , typeof(int[1]) volatile *: 1));
  SASSERT(_Generic(       t8 , typeof(int[1]) volatile *: 1));
  SASSERT(_Generic(typeof(t8), typeof(int[1]) volatile * const: 1));
  SASSERT(_Generic(       t9 , int const volatile *: 1));
}

void array_decay_with_qualifier(void) {
  typeof(int[1]) t1;
  typeof(int[1]) const t2;
  typeof(int[1][1]) t3;
  typeof(int[1][1]) const t4;
  typeof(int[1]) t5[1];
  typeof(int[1]) volatile t7[1];

  typeof(const int[1]) volatile t9;

  SASSERT(_Generic(t1, int *: 1));
  SASSERT(_Generic(t2, int const *: 1));
  SASSERT(_Generic(t3, typeof(int[1]) *: 1));
  SASSERT(_Generic(t4, typeof(int[1]) const *: 1));
  SASSERT(_Generic(t5, typeof(int[1]) *: 1));
  SASSERT(_Generic(t7, typeof(int[1]) volatile *: 1));
  SASSERT(_Generic(t9, int const volatile *: 1));
}

int main(int argc, char**argv) {

  ASSERT(1, ({ char c; _Generic(c << 1, int:1 ); }) );
  ASSERT(1, ({ short s; _Generic(s >> 1, int:1 ); }) );
  ASSERT(1, ({ struct { unsigned int c : 17; } s; _Generic(~s.c, int:1 ); }) );
  ASSERT(1, ({ struct { unsigned long long c : 17; } s; _Generic(~s.c, int:1 ); }) );

  ASSERT(1, ({ char c; _Generic(-c, int:1 ); }) );
  ASSERT(1, ({ short s; _Generic(+s, int:1 ); }) );

  SASSERT(1 == _Generic((long){0}, long:1, long long:0) );
  SASSERT(1 == _Generic((long long){0}, long long:1, long:0) );

  SASSERT(1 == _Generic(1L, long:1, long long:0) );
  SASSERT(1 == _Generic(1LL, long long:1, long:0) );

  SASSERT(1 == _Generic(1UL, unsigned long:1, unsigned long long:0) );
  SASSERT(1 == _Generic(1lu, unsigned long:1, unsigned long long:0) );

  SASSERT(1 == _Generic(1ULL, unsigned long long:1, unsigned long:0) );
  SASSERT(1 == _Generic(1llu, unsigned long long:1, unsigned long:0) );

  ASSERT(1, ({ unsigned long a; long long b; _Generic(a + b, unsigned long long:1, unsigned long:0); }) );

  SASSERT(1 == _Generic(argc ? 0 : "a", char*:1 ) );
  SASSERT(1 == _Generic(argc ? "a" : 0, char*:1 ) );

  SASSERT(1 == _Generic(argc ? (void*)0 : "a", char*:1 ) );
  SASSERT(1 == _Generic(argc ? "a" : (void*)0, char*:1 ) );

  SASSERT(1 == _Generic(argc ? "a" : (void*)(void*)0, void*:1 ) );
  SASSERT(1 == _Generic(argc ? (void*)(void*)0 : "a", void*:1 ) );

  ASSERT(1, ({ void *p; _Generic(argc ? p : "a", void*:1, char*:2); }) );
  ASSERT(1, ({ void *p; _Generic(argc ? "a" : p, void*:1, char*:2); }) );

  ASSERT(1, ({ double *p; char *q; _Generic(argc ? p : q, void*:1 ); }) );
  ASSERT(1, ({ double *p; char *q; _Generic(argc ? q : p, void*:1 ); }) );

  SASSERT(1 == _Generic(0 ? 1 : (short*)0, short*:1));
  SASSERT(1 == _Generic(0 ?: (short*)0, short*:1));

  SASSERT(1 == _Generic(**argv, unsigned char:0, char:1, signed char:0) );
  SASSERT(1 == _Generic("a", unsigned char*:0, char*:1, signed char*:0) );

  ASSERT(1, ({ int vla[argc]; _Generic(vla, int*:1 ); }) );

#ifdef __slimcc__
  ASSERT(1, ({ int vla[argc]; _Generic(&vla, int(*)[*]:1 ); }) );
  ASSERT(1, ({ int vla[argc]; _Generic(typeof(vla), int[*]:1 ); }) );
  ASSERT(1, _Generic(int[argc][argc], int[][*]:1 ) );
#endif

  SASSERT(0 == _Generic(int[2][argc], int[4][2]:1, default:0));
  SASSERT(1 == _Generic(int[argc][2], int[4][2]:1, default:0));
  SASSERT(1 == _Generic(int[4][argc], int[4][2]:1, default:0));
  SASSERT(0 == _Generic(int[argc][4], int[4][2]:1, default:0));

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

  ASSERT(1, ({ const int i; _Generic(        i, int :1, const int: 2);}));
  ASSERT(2, ({ const int i; _Generic(typeof(i), int :1, const int: 2);}));

  SASSERT(2 == _Generic(const int, int :1, const int: 2) );
  SASSERT(2 == _Generic(volatile int, volatile int: 2, int: 1) );
  SASSERT(2 == _Generic(_Atomic int,  _Atomic int: 2, int :1) );

  SASSERT(1 == _Generic(typeof(*(void*)0)*, void *: 1));

  ASSERT(1, ({ typedef _Atomic int I; _Generic(const I, const _Atomic int: 1); }) );
  ASSERT(1, ({ typedef const int I; _Generic(volatile I[3][7], int const volatile[3][7] : 1); }) );
  ASSERT(1, ({ typedef volatile int I[7]; _Generic(const I[3], int const volatile[3][7] : 1); }) );
  ASSERT(1, ({ typedef volatile int I[3][7]; _Generic(const I, int const volatile[3][7] : 1); }) );

  printf("OK\n");
}
