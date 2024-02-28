#include "test.h"

struct S {
  int a, b;
};

int main() {
  struct S s1 = {11,-22 };
  ASSERT(11, ({ ({ s1; }).a; }));
  ASSERT(-22, ({ ({ s1; }); }).b );

  ASSERT(11, ({ ({ struct S s2 = s1; s2; }).a; }));
  ASSERT(-22, ({ ({ struct S s2 = s1; s2; }); }).b );

  ASSERT(11, ({ ({(struct S){11,22}; }).a; }));
  ASSERT(22, ({ ({(struct S){11,22}; }); }).b );

  ASSERT(sizeof(void*), sizeof ({ int arr[17]; arr; }) );
  ASSERT(sizeof(void*), sizeof ({ int arr[s1.a]; arr; }) );

  ({
    if (s1.a == 11) {
      s1.a = 7;
    } else {
      s1.a = 13;
    }
  });
  ASSERT(7, s1.a);

  printf("OK\n");
  return 0;
}
