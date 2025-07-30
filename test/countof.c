#include "test.h"

int vla(int i) {
  ASSERT(3, i);
  DASSERT(11 == _Countof(int[11][++i]));
  ASSERT(3, i);
  ASSERT(4, _Countof(int[++i][11]));
  ASSERT(4, i);

  int arr[11][i+20][33][i+40];

  DASSERT(11 == _Countof arr);
  ASSERT(24, _Countof *arr);
  DASSERT(33 == _Countof **arr);
  ASSERT(44, _Countof ***arr);

  typedef int tarr[i++][33];
  ASSERT(5, i);

  tarr A;
  ASSERT(4, _Countof A);
  DASSERT(33 == _Countof *A);

  ASSERT(4, _Countof(tarr));

  extern int fn_not_to_be_called(int);
  DASSERT(30 == _Countof(int[30][fn_not_to_be_called(i = 90)]));

  return i;
}

int fn_to_be_called(int i) {
  return i * 7;
}

static int array_param(int n, char p[const n]) {
  return _Countof(p);
}

int main(void) {
  ASSERT(5, vla(3));

  char str1[] = "foobar";

  DASSERT(7 == _Countof str1);
  DASSERT(4 == _Countof "foo");

  ASSERT(42, _Countof(int[fn_to_be_called(6)]));

  ASSERT(13, array_param(13, 0));

  printf("OK\n");
}
