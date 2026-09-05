#include "test.h"

int main(void) {
  {
    struct { int i; } s1, s2;
    //SREJ (s1.i ? s1 : s2).i = 13;
    //SREJ (s1 = s2).i = 13;
    //SREJ (1, s2).i = 13;
    //SREJ ((struct S)s2).i = 13;
#ifdef NOTCLANG
    ({ s2; }).i = 13;
#endif
  }
  {
    struct S { const int i; } a1, b1 = {};
    //SREJ a1 = b1;
    struct { struct { const int i; }; int j; } a2, b2 = {};
    //SREJ a2 = b2;
    struct { struct { const int a[5]; } sa[3]; } a3, b3 = {};
    //SREJ a3 = b3;
    struct { struct S s; } a4, b4 = {};
    //SREJ a4 = b4;
  }

  puts("OK");
}
