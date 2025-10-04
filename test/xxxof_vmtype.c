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

int cond_result_type(bool t, bool f, int m, int n) {
    ASSERT(3, sizeof *(1 ? (char(*)[ ])0 : (char(*)[3])0));
    ASSERT(3, sizeof *(1 ? (char(*)[3])0 : (char(*)[ ])0));
    ASSERT(3, sizeof *(1 ? (char(*)[m])0 : (char(*)[3])0));
    ASSERT(3, sizeof *(1 ? (char(*)[3])0 : (char(*)[m])0));

    ASSERT(3, sizeof *(0 ? (char(*)[ ])0 : (char(*)[3])0));
    ASSERT(3, sizeof *(0 ? (char(*)[3])0 : (char(*)[ ])0));
    ASSERT(3, sizeof *(0 ? (char(*)[m])0 : (char(*)[3])0));
    ASSERT(3, sizeof *(0 ? (char(*)[3])0 : (char(*)[m])0));

    ASSERT(3, sizeof *(t ? (char(*)[ ])0 : (char(*)[3])0));
    ASSERT(3, sizeof *(t ? (char(*)[3])0 : (char(*)[ ])0));
    ASSERT(3, sizeof *(t ? (char(*)[m])0 : (char(*)[3])0));
    ASSERT(3, sizeof *(t ? (char(*)[3])0 : (char(*)[m])0));

    ASSERT(3, sizeof *(f ? (char(*)[ ])0 : (char(*)[3])0));
    ASSERT(3, sizeof *(f ? (char(*)[3])0 : (char(*)[ ])0));
    ASSERT(3, sizeof *(f ? (char(*)[m])0 : (char(*)[3])0));
    ASSERT(3, sizeof *(f ? (char(*)[3])0 : (char(*)[m])0));

    ASSERT(5, sizeof *(1 ? (char(*)[m])0 : (char(*)[n])0));
    ASSERT(5, sizeof *(t ? (char(*)[m])0 : (char(*)[n])0));
    ASSERT(7, sizeof *(0 ? (char(*)[m])0 : (char(*)[n])0));
    ASSERT(7, sizeof *(f ? (char(*)[m])0 : (char(*)[n])0));

    char(*p)[m * n];

    ASSERT(35, sizeof *(t ? 0 : p));
    ASSERT(35, sizeof *(1 ? 0 : p));
    ASSERT(35, sizeof *(f ? 0 : p));
    ASSERT(35, sizeof *(0 ? 0 : p));

    ASSERT(35, sizeof *(t ? p : 0));
    ASSERT(35, sizeof *(1 ? p : 0));
    ASSERT(35, sizeof *(f ? p : 0));
    ASSERT(35, sizeof *(0 ? p : 0));

    ASSERT(35, sizeof *(t ? (void*)0 : p));
    ASSERT(35, sizeof *(1 ? (void*)0 : p));
    ASSERT(35, sizeof *(f ? (void*)0 : p));
    ASSERT(35, sizeof *(0 ? (void*)0 : p));

    ASSERT(35, sizeof *(t ? p : (void*)0));
    ASSERT(35, sizeof *(1 ? p : (void*)0));
    ASSERT(35, sizeof *(f ? p : (void*)0));
    ASSERT(35, sizeof *(0 ? p : (void*)0));

    ASSERT(35, sizeof *(t ? nullptr : p));
    ASSERT(35, sizeof *(1 ? nullptr : p));
    ASSERT(35, sizeof *(f ? nullptr : p));
    ASSERT(35, sizeof *(0 ? nullptr : p));

    ASSERT(35, sizeof *(t ? p : nullptr));
    ASSERT(35, sizeof *(1 ? p : nullptr));
    ASSERT(35, sizeof *(f ? p : nullptr));
    ASSERT(35, sizeof *(0 ? p : nullptr));

    ASSERT(35, sizeof *(t ? (char(*)[])0 : p));
    ASSERT(35, sizeof *(1 ? (char(*)[])0 : p));
    ASSERT(35, sizeof *(f ? (char(*)[])0 : p));
    ASSERT(35, sizeof *(0 ? (char(*)[])0 : p));

    ASSERT(35, sizeof *(t ? p : (char(*)[])0));
    ASSERT(35, sizeof *(1 ? p : (char(*)[])0));
    ASSERT(35, sizeof *(f ? p : (char(*)[])0));
    ASSERT(35, sizeof *(0 ? p : (char(*)[])0));

    ASSERT(3, sizeof *(t ? (char(*)[3])0 : p));
    ASSERT(3, sizeof *(1 ? (char(*)[3])0 : p));
    ASSERT(3, sizeof *(f ? (char(*)[3])0 : p));
    ASSERT(3, sizeof *(0 ? (char(*)[3])0 : p));

    ASSERT(3, sizeof *(t ? p : (char(*)[3])0));
    ASSERT(3, sizeof *(1 ? p : (char(*)[3])0));
    ASSERT(3, sizeof *(f ? p : (char(*)[3])0));
    ASSERT(3, sizeof *(0 ? p : (char(*)[3])0));

    ASSERT(7, sizeof *(t ? (char(*)[n])0 : p));
    ASSERT(7, sizeof *(1 ? (char(*)[n])0 : p));
    ASSERT(35, sizeof *(f ? (char(*)[n])0 : p));
    ASSERT(35, sizeof *(0 ? (char(*)[n])0 : p));

    ASSERT(35, sizeof *(t ? p : (char(*)[m])0));
    ASSERT(35, sizeof *(1 ? p : (char(*)[m])0));
    ASSERT(5, sizeof *(f ? p : (char(*)[m])0));
    ASSERT(5, sizeof *(0 ? p : (char(*)[m])0));


    ASSERT(13, _Countof *(t ? (char(*)[13][n][11][m*n-m][3][m])0 : (char(*)[][m][11][m*n-n][n][9])0));
    ASSERT(13, _Countof *(1 ? (char(*)[13][n][11][m*n-m][3][m])0 : (char(*)[][m][11][m*n-n][n][9])0));
    ASSERT(13, _Countof *(f ? (char(*)[13][n][11][m*n-m][3][m])0 : (char(*)[][m][11][m*n-n][n][9])0));
    ASSERT(13, _Countof *(0 ? (char(*)[13][n][11][m*n-m][3][m])0 : (char(*)[][m][11][m*n-n][n][9])0));

    ASSERT(7, _Countof **(t ? (char(*)[13][n][11][m*n-m][3][m])0 : (char(*)[][m][11][m*n-n][n][9])0));
    ASSERT(7, _Countof **(1 ? (char(*)[13][n][11][m*n-m][3][m])0 : (char(*)[][m][11][m*n-n][n][9])0));
    ASSERT(5, _Countof **(f ? (char(*)[13][n][11][m*n-m][3][m])0 : (char(*)[][m][11][m*n-n][n][9])0));
    ASSERT(5, _Countof **(0 ? (char(*)[13][n][11][m*n-m][3][m])0 : (char(*)[][m][11][m*n-n][n][9])0));

    ASSERT(11, _Countof ***(t ? (char(*)[13][n][11][m*n-m][3][m])0 : (char(*)[][m][11][m*n-n][n][9])0));
    ASSERT(11, _Countof ***(1 ? (char(*)[13][n][11][m*n-m][3][m])0 : (char(*)[][m][11][m*n-n][n][9])0));
    ASSERT(11, _Countof ***(f ? (char(*)[13][n][11][m*n-m][3][m])0 : (char(*)[][m][11][m*n-n][n][9])0));
    ASSERT(11, _Countof ***(0 ? (char(*)[13][n][11][m*n-m][3][m])0 : (char(*)[][m][11][m*n-n][n][9])0));

    ASSERT(30, _Countof ****(t ? (char(*)[13][n][11][m*n-m][3][m])0 : (char(*)[][m][11][m*n-n][n][9])0));
    ASSERT(30, _Countof ****(1 ? (char(*)[13][n][11][m*n-m][3][m])0 : (char(*)[][m][11][m*n-n][n][9])0));
    ASSERT(28, _Countof ****(f ? (char(*)[13][n][11][m*n-m][3][m])0 : (char(*)[][m][11][m*n-n][n][9])0));
    ASSERT(28, _Countof ****(0 ? (char(*)[13][n][11][m*n-m][3][m])0 : (char(*)[][m][11][m*n-n][n][9])0));

    ASSERT(3, _Countof *****(t ? (char(*)[13][n][11][m*n-m][3][m])0 : (char(*)[][m][11][m*n-n][n][9])0));
    ASSERT(3, _Countof *****(1 ? (char(*)[13][n][11][m*n-m][3][m])0 : (char(*)[][m][11][m*n-n][n][9])0));
    ASSERT(3, _Countof *****(f ? (char(*)[13][n][11][m*n-m][3][m])0 : (char(*)[][m][11][m*n-n][n][9])0));
    ASSERT(3, _Countof *****(0 ? (char(*)[13][n][11][m*n-m][3][m])0 : (char(*)[][m][11][m*n-n][n][9])0));

    ASSERT(9, _Countof ******(t ? (char(*)[13][n][11][m*n-m][3][m])0 : (char(*)[][m][11][m*n-n][n][9])0));
    ASSERT(9, _Countof ******(1 ? (char(*)[13][n][11][m*n-m][3][m])0 : (char(*)[][m][11][m*n-n][n][9])0));
    ASSERT(9, _Countof ******(f ? (char(*)[13][n][11][m*n-m][3][m])0 : (char(*)[][m][11][m*n-n][n][9])0));
    ASSERT(9, _Countof ******(0 ? (char(*)[13][n][11][m*n-m][3][m])0 : (char(*)[][m][11][m*n-n][n][9])0));

    return 1;
}

int main(void) {
  ASSERT(3, side_effect(3));

  ASSERT(3, with_cast(3));

  ASSERT(1, cond_result_type(1, 0, 5, 7));

  printf("OK\n");
}
