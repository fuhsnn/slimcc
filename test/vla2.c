#include "test.h"
#include <stdint.h>

void fn(int32_t x){
  typedef int32_t A[++x], B[++x];
  A a1, *a2;
  ASSERT(24, sizeof(a1));
  ASSERT(24, sizeof(*a2));

  x = 17;
  B b1[x++], *b2;
  ASSERT(18, x);
  ASSERT(476, sizeof(b1));
  ASSERT(28, sizeof(*b1));
  ASSERT(28, sizeof(*b2));

  typedef int32_t (*C)[++x];
  ASSERT(19, x);
  C c1[++x], c2;
  ASSERT(20, x);
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

  int (*p[++x])[++x];
  ASSERT(9, x);
}

int fn2(int32_t i) {
  static int32_t (*p)[i];
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

int vm_in_cast(int i) {
  (int(*)[i++])(void *)0;

  int tmp;

  void *p = (int(*)[i++])(tmp = i, (void *)0);

  ASSERT(11, tmp);

  __auto_type p2 = (char(*)[i++])(tmp = i, (void *)0);

  ASSERT(11, sizeof *p2);
  ASSERT(12, tmp);

  (int (*(*)(void))[++i])0;
  ASSERT(13, i);
  return 1;
}

int infer_inner_scope(int i, int j) {
  __auto_type v1 = ({
    (int(*)[i - j + 90])(void *)0;
  });
  __auto_type v2 = ({
    (int(*)[j -= i])(void *)0;
  });
  ASSERT(5, sizeof *v1 / sizeof **v1);
  ASSERT(85, sizeof *v2 / sizeof **v2);
  return j;
}

int zinit(int cnt) {
 {
   int a[cnt*100];
   for (int i = 0; i < 100; i++)
     a[i] = -1;
 }
 {
   int a[cnt*100] = {};
   int chk = 0;

   for (int i = 0; i < 100; i++)
     chk |= a[i];

   ASSERT(0, chk);
 }
 return 1;
}

int vm_in_va_arg(int i, ...) {
  va_list ap;
  va_start(ap, i);
  auto p = va_arg(ap, char(*)[i++]);
  va_end(ap);
  ASSERT(6, i);
  ASSERT(7, **p);
  ASSERT(5, sizeof *p);
  return 1;
}

int main(void){
  fn(5);

  ASSERT(12, fn2(3));
  ASSERT(28, fn2(7));

  ASSERT(44, fn3(11));
  ASSERT(52, fn3(13));

  ASSERT(14, fn4(7));

  ASSERT(1, vm_in_cast(9));

  ASSERT(1, vm_in_va_arg(5, &(char){7}));

  ASSERT(85, infer_inner_scope(137, 222));

  ASSERT(1, zinit(1));

  printf("OK\n");
}
