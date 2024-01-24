#include "test.h"

typedef struct {
  long i[4];
} S1;

void *pass(void *ptr){ return ptr; }

int main(void) {
  ASSERT(1, ({ S1 s = {1,2,3,4}; s; }).i[ ({ S1 s = {}; 0;}) ] );
  ASSERT(2, ({ S1 s = {1,2,3,4}; s; }).i[ ({ S1 s = {}; 1;}) ] );
  ASSERT(3, ({ S1 s = {1,2,3,4}; s; }).i[ ({ S1 s = {}; 2;}) ] );
  ASSERT(4, ({ S1 s = {1,2,3,4}; s; }).i[ ({ S1 s = {}; 3;}) ] );

  unsigned long long complit_p1 = (unsigned long long) pass(&(S1[]){0});
  unsigned long long complit_p2 = (unsigned long long) pass(&(S1[]){0});
  ASSERT(1, complit_p1 != complit_p2);

  unsigned long long alloca_p1 = (unsigned long long) pass(alloca(3));
  unsigned long long alloca_p2 = (unsigned long long) pass(alloca(3));
  unsigned long long alloca_p3 = (unsigned long long) pass(({ alloca(3); }));
  unsigned long long alloca_p4 = (unsigned long long) pass(({ alloca(3); }));
  ASSERT(1, alloca_p1 != alloca_p2);
  ASSERT(1, alloca_p2 != alloca_p3);
  ASSERT(1, alloca_p3 != alloca_p4);

  printf("OK\n");
}
