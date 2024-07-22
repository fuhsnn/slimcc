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

  printf("OK\n");
}
