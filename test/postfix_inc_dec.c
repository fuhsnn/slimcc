#include "test.h"

int main(void) {
  _Bool b = 0;
  ASSERT(0, b--);
  ASSERT(1, b--);
  ASSERT(0, b--);
  ASSERT(1, b--);

  b = 0;
  ASSERT(0, b++);
  ASSERT(1, b++);
  ASSERT(1, b++);
  ASSERT(1, b++);

  float f;
  f = 16777216.0f;
  ASSERT(1, 16777216.0f == f++);

  f = -16777216.0f;
  ASSERT(1, -16777216.0f == f--);

  float f2;
  f = -0.0f;
  f2 = f++;
  f = -0.0f;
  ASSERT(1, !memcmp(&f, &f2, sizeof(float)));

  f = -0.0f;
  f2 = f--;
  f = -0.0f;
  ASSERT(1, !memcmp(&f, &f2, sizeof(float)));

  struct S {
    int pad :2;
    int i : 3;
    unsigned u : 3;
    _Bool b: 1;
  } s;

  ASSERT(-4, (s.i = -4, s.i--));
  ASSERT(3, (s.i = 3, s.i++));
  ASSERT(0, (s.u = 0, s.u--));
  ASSERT(7, (s.u = 7, s.u++));

  s.b = 0;
  ASSERT(0, s.b--);
  ASSERT(1, s.b--);
  ASSERT(0, s.b--);
  ASSERT(1, s.b--);

  s.b = 0;
  ASSERT(0, s.b++);
  ASSERT(1, s.b++);
  ASSERT(1, s.b++);
  ASSERT(1, s.b++);

  printf("OK\n");
}
