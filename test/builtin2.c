#include "test.h"

int va(int i, ...) {
  __builtin_va_list ap;
  __builtin_va_start(ap, (i += 7, i));
  __builtin_va_start(ap);
  __builtin_va_end((i += 13, ap));
  return i;
}

int main(void) {
  ASSERT(30, va(17));

  printf("OK\n");
}
