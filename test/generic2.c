#include "test.h"

int main() {
  { SASSERT(_Generic(0, struct S*:1, float:(struct S{int i;}*)0, int:sizeof(struct S)) == sizeof(struct{int i;})); }
  //SREJ (void)_Generic(0, int: 1, int: 1);
  //SREJ (void)_Generic(0, int: 1, default: 0, int: 1);

  printf("OK\n");
  return 0;
}
