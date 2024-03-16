#include "test.h"

typedef struct {
  long i[4];
} S1;

S1 gen_S1(int i) { S1 s = {i,i+1,i+2,i+3}; return s; }

void *pass(void *ptr){ return ptr; }

typedef struct {
  long i[1]; double d[1];
} M;

void va_fn(int i, ...) {
  va_list ap;
  ASSERT(55, ( va_start(ap,i), va_arg(ap, M).i[ ({ S1 s = {}; 0; }) ] ));
  ASSERT(66, ( va_start(ap,i), va_arg(ap, M).d[ ({ S1 s = {}; 0; }) ] ));
}


int main(void) {
  ASSERT(1, ({ S1 s = {1,2,3,4}; s; }).i[ ({ S1 s = {}; 0;}) ] );
  ASSERT(2, ({ S1 s = {1,2,3,4}; s; }).i[ ({ S1 s = {}; 1;}) ] );
  ASSERT(3, ({ S1 s = {1,2,3,4}; s; }).i[ ({ S1 s = {}; 2;}) ] );
  ASSERT(4, ({ S1 s = {1,2,3,4}; s; }).i[ ({ S1 s = {}; 3;}) ] );

  ASSERT(1, gen_S1(1).i[ ({ S1 s = {}; 0;}) ] );
  ASSERT(2, gen_S1(1).i[ ({ S1 s = {}; 1;}) ] );
  ASSERT(3, gen_S1(1).i[ ({ S1 s = {}; 2;}) ] );
  ASSERT(4, gen_S1(1).i[ ({ S1 s = {}; 3;}) ] );

  ASSERT(5, gen_S1(5).i[ gen_S1(0).i[0] ] );
  ASSERT(6, gen_S1(5).i[ gen_S1(0).i[1] ] );
  ASSERT(7, gen_S1(5).i[ gen_S1(0).i[2] ] );
  ASSERT(8, gen_S1(5).i[ gen_S1(0).i[3] ] );

  ASSERT(5, ({ S1 s = {5,6,7,8}; s; }).i[ gen_S1(0).i[0] ] );
  ASSERT(6, ({ S1 s = {5,6,7,8}; s; }).i[ gen_S1(0).i[1] ] );
  ASSERT(7, ({ S1 s = {5,6,7,8}; s; }).i[ gen_S1(0).i[2] ] );
  ASSERT(8, ({ S1 s = {5,6,7,8}; s; }).i[ gen_S1(0).i[3] ] );

  unsigned long long complit_p1 = (unsigned long long) pass(&(S1[]){0});
  unsigned long long complit_p2 = (unsigned long long) pass(&(S1[]){0});
  unsigned long long complit_p3 = (unsigned long long) pass(pass(&(S1[]){0}));
  unsigned long long complit_p4 = (unsigned long long) pass(pass(&(S1[]){0}));

  ASSERT(1, complit_p1 != complit_p2);
  ASSERT(1, complit_p2 != complit_p3);
  ASSERT(1, complit_p3 != complit_p4);

  unsigned long long alloca_p1 = (unsigned long long) pass(alloca(3));
  unsigned long long alloca_p2 = (unsigned long long) pass(alloca(3));
  unsigned long long alloca_p3 = (unsigned long long) pass(({ alloca(3); }));
  unsigned long long alloca_p4 = (unsigned long long) pass(({ alloca(3); }));
  ASSERT(1, alloca_p1 != alloca_p2);
  ASSERT(1, alloca_p2 != alloca_p3);
  ASSERT(1, alloca_p3 != alloca_p4);

  {
    int *p;
    for (_Bool j = 1; j; j = 0)
      p = &(int){17};
    {
      int i = 33;
    }
    ASSERT(17, *p);
  }

  va_fn(0, (M){55,66});

  printf("OK\n");
}
