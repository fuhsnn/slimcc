#include "test.h"

struct S {
  long i[4];
};

int main(void) {
  ASSERT(1, ({ struct S s = {1,2,3,4}; s; }).i[ ({ struct S s = {}; 0;}) ] );
  ASSERT(2, ({ struct S s = {1,2,3,4}; s; }).i[ ({ struct S s = {}; 1;}) ] );
  ASSERT(3, ({ struct S s = {1,2,3,4}; s; }).i[ ({ struct S s = {}; 2;}) ] );
  ASSERT(4, ({ struct S s = {1,2,3,4}; s; }).i[ ({ struct S s = {}; 3;}) ] );

  printf("OK\n");
}
