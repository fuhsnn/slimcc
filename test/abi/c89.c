#include "test.h"

float mixed1(float f, _Bool, char i);
float mixed2(float f, _Bool, char i);

float mixed2(f, b, c)
char c; float f; _Bool b; {
  return f - b + c;
}

float conv1(
#ifndef __clang__
foo, bar
#endif
);
float conv2(
#ifndef __clang__
foo, bar
#endif
);

float conv2(f, b, c, i)
char c; float f; _Bool b; {
  return f - b + c + i;
}

void *pass(void *p){
  return p;
}

int decl_scope(int t) {
  {
    int *p, *q;
    if (t)
      p = &(int){9};
    if (t)
      q = &(int){7};

    ASSERT(9, *p);
    ASSERT(7, *q);
  }
  {
    enum { A = 5 };
    struct S { char arr[9]; };
    {
      if (sizeof(enum {A = 7}))
        (void)sizeof(struct S { char arr[3]; });
      ASSERT(7, A);
      ASSERT(3, sizeof(struct S));
    }
    ASSERT(5, A);
    ASSERT(9, sizeof(struct S));
    {
      void proto(enum { A = 7 });
      void proto2(struct S { char arr[7]; }*);
      ASSERT(5, A);
      ASSERT(9, sizeof(struct S));
    }
  }
  {
    struct S *p;
    pass(&(struct S { char arr[7]; }){(enum { A = 6 })0});
    ASSERT(7, sizeof(struct S));
    ASSERT(7, sizeof(*p));
    ASSERT(6, A);
  }
  return 1;
}

int main(void) {
  ASSERT(1, decl_scope(1));

  ASSERT(1, 5.125 == mixed1(0.125, 1 << 30, 6));
  ASSERT(1, 5.125 == mixed2(0.125, 1 << 30, 6));

  ASSERT(1, 16.125 == conv1(0.125, 1 << 30, 6, 10));
  ASSERT(1, 16.125 == conv2(0.125, 1 << 30, 6, 10));

  printf("OK\n");
  return 0;
}
