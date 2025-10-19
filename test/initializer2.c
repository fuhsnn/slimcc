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

long long z10 = {};
static long long z11 = {};

int c23_zinit(void) {
  long long z20 = {};
  static long long z21 = {};

  ASSERT(0, z10);
  ASSERT(0, z11);

  ASSERT(0, z20);
  ASSERT(0, z21);

  return 1;
}

int flexible_structs(void) {
  struct S {
    char c;
    char arr[];
  };

  static struct S arr1[] = {1, {}, 2, {}, 3};
  ASSERT(1, arr1[0].c);
  ASSERT(2, arr1[1].c);
  ASSERT(3, arr1[2].c);
  ASSERT(2, &arr1[2] - &arr1[0]);

  static struct S s3 = {42, {}};

  struct S arr2[3];
  arr2[2].c = arr1[0].c;
  arr2[1] = arr1[2];
  arr2[0] = s3;
  ASSERT(42, arr2[0].c);
  ASSERT(3, arr2[1].c);
  ASSERT(1, arr2[2].c);

  union U {
    char arr[2];
    struct S s;
  };
  union U u1 = {0};
  ASSERT(0, u1.arr[0]);
  ASSERT(0, u1.arr[1]);
  u1.s = s3;
  ASSERT(42, u1.arr[0]);
  ASSERT(0, u1.arr[1]);

  union U u2 = {.s = s3};
  ASSERT(42, u2.arr[0]);

  return 1;
}

static int gnu_array_range(void) {
  {
    int i[2][4] = {[0][0 ... 1] = 4, 2, 3, 7, [1][1 ... 2] = 1, 9};
    int ans[] = {4,4,2,3,7,1,1,9};
    ASSERT(0, memcmp(&i, &ans, sizeof(ans)));
  }

#ifdef NOTCLANG
  {
    struct S { int a; int b; };
    int i;
    i = 1;
    struct { struct S s[4]; } arr1[5] = { [2 ... 3].s[1 ... 2] = { i += 2, i *= 3} };
    struct { struct S s[4]; } ans1[5] = {0};
    ans1[2].s[1] = ans1[2].s[2] = ans1[3].s[1] = ans1[3].s[2] = (struct S){3, 9};
    ASSERT(0, memcmp(&arr1, &ans1, sizeof(arr1)));
    ASSERT(9, i);

    i = 1;
    struct { struct S s[3]; } arr2[5] = { [1 ... 3].s[1] = {i++, ++i} };
    struct { struct S s[3]; } ans2[5] = {0};
    ans2[1].s[1] = ans2[3].s[1] = ans2[2].s[1] = (struct S){1, 3};
    ASSERT(0, memcmp(&arr2, &ans2, sizeof(arr2)));
    ASSERT(3, i);

    i = 1;
    struct { int i[4]; } arr3[5] = { [4].i[1 ... 2] = ({ int j = i + 7; j - i; }) };
    struct { int i[4]; } ans3[5] = {0};
    ans3[4].i[1] = ans3[4].i[2] = 7;
    ASSERT(0, memcmp(&arr3, &ans3, sizeof(arr3)));
    ASSERT(1, i);

    {
      struct S2 { struct { struct { struct { int i[3]; }s[3]; }s[3]; }s[3]; };
      struct S2 s = { .s[0 ... 1].s[1 ... 2].s[0 ... 1].i[1 ... 2] = i*=7 };
      struct S2 ans = {0};
      ans.s[0].s[1].s[0].i[1] = ans.s[0].s[1].s[0].i[2] = ans.s[0].s[1].s[1].i[1] =
      ans.s[0].s[1].s[1].i[2] = ans.s[0].s[2].s[0].i[1] = ans.s[0].s[2].s[0].i[2] =
      ans.s[0].s[2].s[1].i[1] = ans.s[0].s[2].s[1].i[2] = ans.s[1].s[1].s[0].i[1] =
      ans.s[1].s[1].s[0].i[2] = ans.s[1].s[1].s[1].i[1] = ans.s[1].s[1].s[1].i[2] =
      ans.s[1].s[2].s[0].i[1] = ans.s[1].s[2].s[0].i[2] = ans.s[1].s[2].s[1].i[1] =
      ans.s[1].s[2].s[1].i[2] = 7;
      ASSERT(0, memcmp(&s, &ans, sizeof(struct S2)));
      ASSERT(7, i);

#ifdef __slimcc__
      i = 1;
      struct S2 s2 = { .s[0 ... 1].s[1 ... 2].s[0 ... 1].i = { [1 ... 2] = i *= 7} };
      ASSERT(0, memcmp(&s2, &ans, sizeof(struct S2)));
      ASSERT(7, i);

      i = 1;
      struct S2 s3 = { .s[0 ... 1].s[1 ... 2].s[0 ... 1] = { .i[1 ... 2] = i *= 7} };
      ASSERT(0, memcmp(&s3, &ans, sizeof(struct S2)));
      ASSERT(7, i);

      i = 1;
      struct S2 s4 = { .s[0 ... 1].s[1 ... 2].s = { [0 ... 1].i[1 ... 2] = i *= 7} };
      ASSERT(0, memcmp(&s4, &ans, sizeof(struct S2)));
      ASSERT(7, i);

      i = 1;
      struct S2 s5 = { .s[0 ... 1].s[1 ... 2] = { .s[0 ... 1].i[1 ... 2] = i *= 7} };
      ASSERT(0, memcmp(&s5, &ans, sizeof(struct S2)));
      ASSERT(7, i);

      i = 1;
      struct S2 s6 = { .s[0 ... 1].s = { [1 ... 2].s[0 ... 1].i[1 ... 2] = i *= 7} };
      ASSERT(0, memcmp(&s6, &ans, sizeof(struct S2)));
      ASSERT(7, i);

      i = 1;
      struct S2 s7 = { .s[0 ... 1].s[1 ... 2] = { .s[0 ... 1].i[1 ... 2] = i *= 7} };
      ASSERT(0, memcmp(&s7, &ans, sizeof(struct S2)));
      ASSERT(7, i);

      i = 1;
      struct S2 s8 = { .s[0 ... 1].s = { [1 ... 2].s[0 ... 1].i[1 ... 2] = i *= 7} };
      ASSERT(0, memcmp(&s8, &ans, sizeof(struct S2)));
      ASSERT(7, i);

      i = 1;
      struct S2 s9 = { .s[0 ... 1] = { .s[1 ... 2].s[0 ... 1].i[1 ... 2] = i *= 7} };
      ASSERT(0, memcmp(&s9, &ans, sizeof(struct S2)));
      ASSERT(7, i);

      i = 1;
      struct S2 s10 = { .s = { [0 ... 1].s[1 ... 2].s[0 ... 1].i[1 ... 2] = i *= 7} };
      ASSERT(0, memcmp(&s10, &ans, sizeof(struct S2)));
      ASSERT(7, i);
#endif
    }
  }
#endif
  return 1;
}

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
  ASSERT(1, c23_zinit());
  ASSERT(1, flexible_structs());
  ASSERT(1, gnu_array_range());

  printf("OK\n");
}

