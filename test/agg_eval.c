#include "test.h"

struct S2 {
  int i,j;
};
struct S3 {
  char c;
  struct S2 arr[2];
};
struct S3 s3 = { .arr = {3, 7, 9, 11} };
struct S2 *s2p = {(struct S2 *)((struct S2 const *)&(((struct S3 const *)&s3))->arr + 1)};

int static_ref_excess_cast(void) {
  return s2p->j;
}

int aggregate_eval(void) {
  {
    struct S {
      int arr[2];
    };
    struct S *src[2] = {&(struct S){5,6}, &(struct S){7,8}};
    int *p = *&src[1]->arr;
    ASSERT(7, *p);
  }
  {
    struct S {
      int i;
      int arr[3];
    };
    struct { struct S *p; } s = {&(struct S){1,2,3,4}};
    ASSERT(4, s.p->arr[2]);
  }
  {
    struct S {
      int arr[2][2];
    };
    struct S s = {1,2,3,4};

    struct { struct S *p; } ps = {&s};
    int *p = *&(ps.p->arr)[1];
    ASSERT(3, *p);
  }
  return 1;
}

int main(void) {
  {
    struct S { struct { char c, c2; } s[13]; };
    DASSERT(13 == (intptr_t)&((struct S*)0)->s[6].c2);
  }
  {
    struct S { char a; _Atomic char b; volatile char c; };
    SASSERT(1 ==  offsetof(struct S, b));
    SASSERT(2 ==  offsetof(struct S, c));
  }

  ASSERT(11, static_ref_excess_cast());
  ASSERT(1, aggregate_eval());

  printf("OK\n");
  return 0;
}
