#include "test.h"

#define CAT(x) b##x
int CAT(6_) = 7;
int CAT(8µ) = 9;

int fn(float), var1 = 7, fn2(void), var2 = 11;

int tentative_var;
int tentative_var;

short incomplete_tentative_arr[];

extern int init_extern = 3;

static inline int static_inline_fn(void);
int (*static_inline_fn_ptr)(void) = static_inline_fn;

int main(void) {
  {
    ASSERT(7, b6_);
    ASSERT(9, b8µ);
  }

  {
    ASSERT(7, var1);
    ASSERT(11, var2);
    ASSERT(13, fn(13.0f));
    ASSERT(19, fn2());
  }
  {
    char arr2[3] = {11, 22, 33};
    ASSERT(33, (&arr2)[0][2]);
    ASSERT(22, (*&arr2)[1]);
    ASSERT(11, (**&arr2));
  }

  ASSERT(0, tentative_var);

  ASSERT(0, incomplete_tentative_arr[0]);

  ASSERT(3, init_extern);

  ASSERT(127, static_inline_fn_ptr());

  ASSERT(0, ({ struct S {}; sizeof(struct S);}));
  ASSERT(0, ({ union U {}; sizeof(union U);}));

  printf("OK\n");
}


int fn(float f) { return f; }
int fn2(void) { return 19; }
static inline int static_inline_fn(void) {  return 127; }
