#include "test.h"

int x;
int x = 5;
int y = 7;
int y;
int common_ext1;
static int common_local;

int main() {
  ASSERT(5, x);
  ASSERT(7, y);
  ASSERT(0, common_ext1);

  printf("OK\n");
  return 0;
}
