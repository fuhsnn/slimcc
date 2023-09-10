#include "test.h"

void fn(int x){
  typedef int A[++x], B[++x];
  A a1, *a2;
  ASSERT(24, sizeof(a1));
  ASSERT(24, sizeof(*a2));

  x = 17;
  B b1[x++], *b2;
  ASSERT(476, sizeof(b1));
  ASSERT(28, sizeof(*b1));
  ASSERT(28, sizeof(*b2));

  typedef int (*C)[++x];
  C c1[++x], c2;
  ASSERT(160, sizeof(c1));
  ASSERT(8, sizeof(*c1));
  ASSERT(76, sizeof(**c1));
  ASSERT(76, sizeof(*c2));

  typedef char D[x = 177];
  D a;
  x = 7;
  D b;
  D c;

  ASSERT(192, (&a[0] - &b[0]));
  ASSERT(192, (&b[0] - &c[0]));
}

int main(void){
  fn(5);

  printf("OK\n");
}
