#include "test.h"

typedef struct {} S;

static S zero_sized_rtn(int *i, int v) {
  return (*i = 10 + v, (S){});
}

int main(void) {
  struct {} s, s2 = {};
  union {} u, u2 = {};
  static typeof(s) ss, ss2 = {};
  static typeof(u) su, su2 = {};
  short a[0] = {};
  struct {short a[0];} sa = {};

  SASSERT(0 == sizeof(s));
  SASSERT(1 == _Alignof(s));
  SASSERT(0 == sizeof(u));
  SASSERT(1 == _Alignof(u));
  SASSERT(0 == sizeof(a));
  SASSERT(_Alignof(short) == _Alignof(a));
  SASSERT(0 == sizeof(sa));
  SASSERT(_Alignof(short) == _Alignof(sa));

  SASSERT(_Generic(s, S:0, default:1));

  ASSERT(1, &s == (&s + 1));
  ASSERT(1, &u == (&u + 1));
  ASSERT(1, &a == (&a + 1));
  ASSERT(1, &sa == (&sa + 1));

  int i = 0;
  *(i++, &s) = (i++, s2);
  *(i++, &ss) = (i++, s2);
  *(i++, &u) = (i++, u2);
  *(i++, &su) = (i++, u2);
  ASSERT(8, i);

  ASSERT(11, ({ int i; zero_sized_rtn(&i, 1); i; }));
  ASSERT(12, ({ int i; S s = zero_sized_rtn(&i, 2); i; }));
  ASSERT(13, ({ int i; struct { S s; } s = {zero_sized_rtn(&i, 3)}; i; }));
  ASSERT(29, ({ int i, j; struct { S s, s2; } s = {zero_sized_rtn(&i, 4), zero_sized_rtn(&j, 5)}; i + j; }));

  {
    int i;
    struct {
      int8_t x;
      S s;
      int8_t y;
    } s = {-1, zero_sized_rtn(&i, 6), -2};

    ASSERT(-1, s.x);
    ASSERT(16, i);
    ASSERT(-2, s.y);
    SASSERT(2 == sizeof(s));
    ASSERT(1, (void *)&s.s == (void *)&s.y);
  }

  printf("OK\n");
}
