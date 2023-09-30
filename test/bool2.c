#include "test.h"

int main(void) {
  _Bool x, y ,b;
  b = 0;
  x = b, y = b--;
  ASSERT(x, 0);
  ASSERT(y, 0);
  x = b, y = b--;
  ASSERT(x, 1);
  ASSERT(y, 1);
  x = b, y = b--;
  ASSERT(x, 0);
  ASSERT(y, 0);

  b = 0;
  x = b, y = b++;
  ASSERT(x, 0);
  ASSERT(y, 0);
  x = b, y = b++;
  ASSERT(x, 1);
  ASSERT(y, 1);
  x = b, y = b++;
  ASSERT(x, 1);
  ASSERT(y, 1);

  printf("OK\n");
}
