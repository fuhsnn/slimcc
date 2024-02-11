#include "test.h"
#include <stddef.h>

typedef struct {
  int a;
  char b;
  int c;
  double d;
} T;

int main() {
  EASSERT(0, offsetof(T, a));
  EASSERT(4, offsetof(T, b));
  EASSERT(8, offsetof(T, c));
  EASSERT(16, offsetof(T, d));

  printf("OK\n");
  return 0;
}
