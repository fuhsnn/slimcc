#include "test.h"

long double ld = 0.0L;

#define CAT(x) b##x
int CAT(6_) = 7;
int CAT(8µ) = 9;

const int arr[2][2] = {{1,2},{3,4}};
int *const p1 = arr[1];

int fn(float), var1 = 7, fn2(void), var2 = 11;

int tentative_var;
int tentative_var;

short incomplete_tentative_arr[];

extern int init_extern = 3;

static inline int static_inline_fn(void);
int (*static_inline_fn_ptr)(void) = static_inline_fn;

int g1 = ((int){77});
int g2 = arr[1][1];

const struct {
  int *a, *b, *c;
} relo = {.a = &g1, .c = &g2};
int *p2 = relo.c;
int *p1p = p1;

struct S { int i, j, k; };
struct S const s1 = {99, 88, 77};
struct S s2 = (struct S){33,44,55};
struct S s3 = s1;
#ifdef NOTGCC
int g3 = (struct S){55,66,77}.j;
int g4 = (const int[]){11,22,33,44}[2];
#endif

static long long obar = 2 + (long long)"foobar";

int main(void) {
  {
    ASSERT(7, b6_);
    ASSERT(9, b8µ);
  }
  {
    ASSERT(3, *p1);
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

  ASSERT(77, g1);
  ASSERT(4, g2);
  ASSERT(1, p2 == &g2);
  ASSERT(1, p1p == p1);

  ASSERT(33, s2.i);
  ASSERT(44, s2.j);
  ASSERT(55, s2.k);

  ASSERT(99, s3.i);
  ASSERT(88, s3.j);
  ASSERT(77, s3.k);

#ifdef NOTGCC
  ASSERT(66, g3);
  ASSERT(33, g4);
#endif

  ASSERT(0, strcmp("obar", (char *)obar));

  printf("OK\n");
}


int fn(float f) { return f; }
int fn2(void) { return 19; }
static inline int static_inline_fn(void) {  return 127; }
