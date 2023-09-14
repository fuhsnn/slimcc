#include "test.h"
#include <stdint.h>

void fn(int32_t x){
  typedef int32_t A[++x], B[++x];
  A a1, *a2;
  ASSERT(24, sizeof(a1));
  ASSERT(24, sizeof(*a2));

  x = 17;
  B b1[x++], *b2;
  ASSERT(476, sizeof(b1));
  ASSERT(28, sizeof(*b1));
  ASSERT(28, sizeof(*b2));

  typedef int32_t (*C)[++x];
  C c1[++x], c2;
  ASSERT(160, sizeof(c1));
  ASSERT(8, sizeof(*c1));
  ASSERT(76, sizeof(**c1));
  ASSERT(76, sizeof(*c2));

  typedef int8_t D[x = 177];
  D a;
  x = 7;
  D b;
  D c;

  ASSERT(192, (&a[0] - &b[0]));
  ASSERT(192, (&b[0] - &c[0]));
}

int fn2(int32_t i) {
  static (*p)[i];
  return sizeof *p;
}

int fn3(int i) {
  typedef int32_t (*T)[i];
  static T t;
  return sizeof *t;
}

int fn4(int i){
  return sizeof(*(char(*)[i+7]){0});
}

int main(void){
  fn(5);

  ASSERT(12, fn2(3));
  ASSERT(28, fn2(7));

  ASSERT(44, fn3(11));
  ASSERT(52, fn3(13));

  ASSERT(14, fn4(7));


  printf("OK\n");
}
