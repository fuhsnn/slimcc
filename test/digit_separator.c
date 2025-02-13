#include "test.h"

int main(void) {

#define CAT(A,B) A##B

  constexpr int i1 = CAT(0x3'5, 6);
  EASSERT(i1, 854);

  printf("OK\n");
}
