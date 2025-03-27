#include "test.h"

long double ld = 0.0L;

const int arr[2][2] = {{1,2},{3,4}};
int *const p1 = arr[1];

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

_Bool b = 2;
_Bool ba[] = {3,3,3};
struct {
    _Bool b;
} bs = {4};

int main(void) {
  ASSERT(3, *p1);

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

  ASSERT(1, b);
  ASSERT(1, ba[2]);
  ASSERT(1, bs.b);

  ASSERT(0, strcmp("obar", (char *)obar));

  {
    int i[2][4] = {[0][0 ... 1] = 4, 2, 3, 7, [1][1 ... 2] = 1, 9};
    int ans[] = {4,4,2,3,7,1,1,9};
    ASSERT(0, memcmp(&i, &ans, sizeof(ans)));
  }
  {
    struct Sub {
      int i;
    };
    struct S {
      struct Sub b;
      int j;
    };
    struct Sub b = {3};
    struct S s[] = {b, 6, 7};
    ASSERT(3, s[0].b.i);
    ASSERT(6, s[0].j);
    ASSERT(7, s[1].b.i);
    ASSERT(0, s[1].j);
  }

  printf("OK\n");
}

