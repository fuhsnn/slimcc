#include "test.h"

int main(int argc, char**argv) {

  EASSERT(1, _Alignof (char [argc]));

  char _Alignas(1024) arr1[11];
  char _Alignas(1024) arr2[argc];
  EASSERT(1024, _Alignof arr1);
  EASSERT(1, _Alignof arr1[0]);

  EASSERT(1024, _Alignof arr2);
  EASSERT(1, _Alignof arr2[0]);

  struct {
    char c;
    _Alignas(1024) int i;
  } s;
  EASSERT(1024, _Alignof s);
  EASSERT(1, _Alignof s.c);
  EASSERT(1024, _Alignof s.i);

  EASSERT(1, _Alignof (char){1});

  printf("OK\n");
}
