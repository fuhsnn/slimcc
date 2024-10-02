#include "test.h"

// Adapted from https://www.open-std.org/JTC1/SC22/WG14/www/docs/n3199.htm

#ifdef defer
#error
#endif

#define defer _Defer

int n3199_ex2(void) {
  int r = 4;
  int* p = &r;
  defer { *p = 5; }
  return *p;
}

int n3199_ex5(void) {
  int r = 0;
  {
    defer {
      defer r *= 4;
      r *= 2;
      defer {
        r += 3;
      }
    }
    defer r += 1;
  }
  return r;
}

void n3199_ex4(void) {
  char buf[64] = "c";
  {
    defer {
      snprintf(buf, 64, "%s meow", strdup(buf));
    }
    if (1)
      defer snprintf(buf, 64, "%sat", strdup(buf));
    snprintf(buf, 64, "%s says", strdup(buf));
  }
  ASSERT(0, strcmp(buf, "cat says meow"));
}

int main () {
  ASSERT(4, n3199_ex2());
  n3199_ex4();
  ASSERT(20, n3199_ex5());

  printf("OK\n");
  return 0;
}
