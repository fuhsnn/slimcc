#include "test.h"


static constexpr float f = 11.0;
constexpr long double ld = 22.0;

constexpr long len = 17;
long double arr0[len];

void local_adr(int recur, const int *local_p, const int *static_p) {
  if (recur >= 3)
    return;

  constexpr int local_v = 55;
  static constexpr int static_v  = 66;

  DASSERT(55 == local_v);
  DASSERT(66 == static_v);

  if (recur) {
    ASSERT(55, *local_p);
    ASSERT(66, *static_p);

    ASSERT(1, &local_v != local_p );
    ASSERT(1, &static_v == static_p );
  }
  local_adr(recur + 1, &local_v, &static_v);
}

int main() {
  DASSERT(ld == f * 2);
  DASSERT(sizeof(arr0) == len * sizeof(typeof(arr0[0])));

  local_adr(0,NULL,NULL);

  union U1 { int i; union { int j; union { int k; int m; }; }; };
  constexpr union U1 u1 = {.m =77 };
  DASSERT( u1.m == 77 );

   struct S {
     char c;
     struct {
       float f;
     }a;
     struct {
       unsigned short w :6;
       short x :7;
       struct {
         long double d;
       };
     };
   };
  constexpr struct S s = { 11,22,33,-44,55 };

  DASSERT(s.c == 11);
  DASSERT(s.a.f == 22);
  DASSERT(s.w == 33);
  DASSERT(s.x == -44);
  DASSERT(s.d == 55);

  {
    struct S { int i; };
    constexpr _Bool b = (0 && (((struct S*)0)->i));
  }
  {
    constexpr int32_t x = -6L;
    constexpr uint64_t y = (uint64_t)x;
    static_assert((uint64_t)-6L == y);
  }
  {
    constexpr struct {  char c[23]; } s = {0};
    static_assert((s,1));
  }

  for (constexpr int i = 0; i;) {
    static_assert(i == 0);
  }

  printf("OK\n");
  return 0;
}

