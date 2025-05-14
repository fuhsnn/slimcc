#include "test.h"

struct S { void *p; };

const __attribute__((section("sec1"))) struct S const_no_relo = {0};
const __attribute__((section("sec1"))) struct S const_with_relo = {.p = test_assert};

int main(void) {
  printf("OK\n");
}
