#include "test.h"

_Noreturn void noreturn_fn(int volatile x) {
  exit(0);
}

void funcy_type(int arg[restrict static 3]) {}

int main() {
  { int volatile x; }
  { volatile int x; }
  { volatile int volatile volatile x; }
  { int volatile * volatile volatile x; }

  printf("OK\n");
  return 0;
}
