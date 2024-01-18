#include "test.h"

typedef struct {
  long i[4];
} S1;

S1 *sel(_Bool b, S1*p1, S1*p2) { return b ? p1 : p2; }
S1 *pass(S1 *ptr){ return ptr; }

int main(void) {
  ASSERT(1, ({ S1 s = {1,2,3,4}; s; }).i[ ({ S1 s = {}; 0;}) ] );
  ASSERT(2, ({ S1 s = {1,2,3,4}; s; }).i[ ({ S1 s = {}; 1;}) ] );
  ASSERT(3, ({ S1 s = {1,2,3,4}; s; }).i[ ({ S1 s = {}; 2;}) ] );
  ASSERT(4, ({ S1 s = {1,2,3,4}; s; }).i[ ({ S1 s = {}; 3;}) ] );

  ASSERT(1, sel(1, pass(&(S1){1,2,3,4}), pass(&(S1){5,6,7,8}))->i[0]);
  ASSERT(2, sel(1, pass(&(S1){1,2,3,4}), pass(&(S1){5,6,7,8}))->i[1]);
  ASSERT(3, sel(1, pass(&(S1){1,2,3,4}), pass(&(S1){5,6,7,8}))->i[2]);
  ASSERT(4, sel(1, pass(&(S1){1,2,3,4}), pass(&(S1){5,6,7,8}))->i[3]);
  ASSERT(5, sel(0, pass(&(S1){1,2,3,4}), pass(&(S1){5,6,7,8}))->i[0]);
  ASSERT(6, sel(0, pass(&(S1){1,2,3,4}), pass(&(S1){5,6,7,8}))->i[1]);
  ASSERT(7, sel(0, pass(&(S1){1,2,3,4}), pass(&(S1){5,6,7,8}))->i[2]);
  ASSERT(8, sel(0, pass(&(S1){1,2,3,4}), pass(&(S1){5,6,7,8}))->i[3]);

  printf("OK\n");
}
