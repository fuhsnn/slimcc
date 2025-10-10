#include "test.h"

int side_effect(int i) {

#define TEST(_type, _stmt) ({ \
    _type p = 0;              \
    _stmt;                    \
    p != 0;                   \
  })

  int dummy;
  {
    ASSERT(0, TEST(typeof (int[4][i])*, dummy = sizeof(p++)));

    ASSERT(1, TEST(typeof (int[4][i])*, dummy = sizeof(*p++)));
    ASSERT(0, TEST(typeof (int[4][i])*, dummy = _Countof(*p++)));

    ASSERT(1, TEST(typeof (int[4][i])*, dummy = sizeof(**p++)));
    ASSERT(1, TEST(typeof (int[4][i])*, dummy = _Countof(**p++)));

    ASSERT(0, TEST(typeof (int[4][i])*, dummy = sizeof(***p++)));
  }
  {
    ASSERT(0, TEST(typeof (int[i][2])*, dummy = sizeof(p++)));

    ASSERT(1, TEST(typeof (int[i][2])*, dummy = sizeof(*p++)));
    ASSERT(1, TEST(typeof (int[i][2])*, dummy = _Countof(*p++)));

    ASSERT(0, TEST(typeof (int[i][2])*, dummy = sizeof(**p++)));
    ASSERT(0, TEST(typeof (int[i][2])*, dummy = _Countof(**p++)));

    ASSERT(0, TEST(typeof (int[i][2])*, dummy = sizeof(***p++)));
  }
  return i;
}

int with_cast(int idx) {
  SASSERT(1 == sizeof(char));
  int z = sizeof (char *);
  {
    int i = idx;
    ASSERT(z, sizeof((char(*)[i+=2])(i+=2, (void*)0)));
    ASSERT(3, i);

    ASSERT(5, sizeof(*(char(*)[i+=2])(i+=2, (void*)0)));
    ASSERT(7, i);

    ASSERT(1, sizeof(**(char(*)[i+=2])(i+=2, (void*)0)));
    ASSERT(7, i);
  }
  {
    int i = idx;
    ASSERT(z, sizeof((char(*)[i+=2][2])(i+=2, (void*)0)));
    ASSERT(3, i);

    ASSERT(10, sizeof(*(char(*)[i+=2][2])(i+=2, (void*)0)));
    ASSERT(7, i);

    ASSERT(2, sizeof(**(char(*)[i+=2][2])(i+=2, (void*)0)));
    ASSERT(7, i);

    ASSERT(1, sizeof(***(char(*)[i+=2][2])(i+=2, (void*)0)));
    ASSERT(7, i);
  }
  {
    int i = idx;
    ASSERT(z, sizeof((char(*)[2][i+=2])(i+=2, (void*)0)));
    ASSERT(3, i);

    ASSERT(10, sizeof(*(char(*)[2][i+=2])(i+=2, (void*)0)));
    ASSERT(7, i);

    ASSERT(9, sizeof(**(char(*)[2][i+=2])(i+=2, (void*)0)));
    ASSERT(11, i);

    ASSERT(1, sizeof(***(char(*)[2][i+=2])(i+=2, (void*)0)));
    ASSERT(11, i);
  }
  {
    int i = idx;
    ASSERT(z, sizeof((i+=2, (char(*)[i+=2])0)));
    ASSERT(3, i);

    ASSERT(7, sizeof(*(i+=2, (char(*)[i+=2])0)));
    ASSERT(7, i);

    ASSERT(1, sizeof(**(i+=2, (char(*)[i+=2])0)));
    ASSERT(7, i);
  }
  {
    int i = idx;
    ASSERT(z, sizeof((i+=2, (char(*)[i+=2][2])0)));
    ASSERT(3, i);

    ASSERT(14, sizeof(*(i+=2, (char(*)[i+=2][2])0)));
    ASSERT(7, i);

    ASSERT(2, sizeof(**(i+=2, (char(*)[i+=2][2])0)));
    ASSERT(7, i);

    ASSERT(1, sizeof(***(i+=2, (char(*)[i+=2][2])0)));
    ASSERT(7, i);
  }
  {
    int i = idx;
    ASSERT(z, sizeof((i+=2, (char(*)[2][i+=2])0)));
    ASSERT(3, i);

    ASSERT(14, sizeof(*(i+=2, (char(*)[2][i+=2])0)));
    ASSERT(7, i);

    ASSERT(11, sizeof(**(i+=2, (char(*)[2][i+=2])0)));
    ASSERT(11, i);

    ASSERT(1, sizeof(***(i+=2, (char(*)[2][i+=2])0)));
    ASSERT(11, i);
  }
  {
    int i = idx;
    ASSERT(5, _Countof(*(char(*)[i+=2])(i+=2, (void*)0)));
    ASSERT(7, i);
  }
  {
    int i = idx;
    ASSERT(5, _Countof(*(char(*)[i+=2][2])(i+=2, (void*)0)));
    ASSERT(7, i);
    ASSERT(2, _Countof(**(char(*)[i+=2][2])(i+=2, (void*)0)));
    ASSERT(7, i);
  }
  {
    int i = idx;
    ASSERT(2, _Countof(*(char(*)[2][i+=2])(i+=2, (void*)0)));
    ASSERT(3, i);
    ASSERT(5, _Countof(**(char(*)[2][i+=2])(i+=2, (void*)0)));
    ASSERT(7, i);
  }
  {
    int i = idx;
    ASSERT(7, _Countof(*(i+=2, (char(*)[i+=2])0)));
    ASSERT(7, i);
  }
  {
    int i = idx;
    ASSERT(7, _Countof(*(i+=2, (char(*)[i+=2][2])0)));
    ASSERT(7, i);
    ASSERT(2, _Countof(**(i+=2, (char(*)[i+=2][2])0)));
    ASSERT(7, i);
  }
  {
    int i = idx;
    ASSERT(2, _Countof(*(i+=2, (char(*)[2][i+=2])0)));
    ASSERT(3, i);
    ASSERT(7, _Countof(**(i+=2, (char(*)[2][i+=2])0)));
    ASSERT(7, i);
  }
  return idx;
}

int main(void) {
  ASSERT(3, side_effect(3));

  ASSERT(3, with_cast(3));

  printf("OK\n");
}
