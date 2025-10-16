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

int decl_scope(void) {
  {
    enum { A = 5 };
    struct S { char arr[9]; };
    {
      if (sizeof(enum {A = 7}))
        (void)sizeof(struct S { char arr[3]; });
      ASSERT(5, A);
      ASSERT(9, sizeof(struct S));
    }
    ASSERT(5, A);
    ASSERT(9, sizeof(struct S));
    {
      void proto(enum { A = 7 });
      void proto2(struct S { char arr[7]; }*);
      ASSERT(5, A);
      ASSERT(9, sizeof(struct S));
    }
  }
  {
    struct S *p;
    pass(&(struct S { char arr[7]; }){(enum { A = 6 })0});
    ASSERT(7, sizeof(struct S));
    ASSERT(7, sizeof(*p));
    ASSERT(6, A);
  }
  return 1;
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

  va_fn(0, (M){55,66});

  ASSERT(1, decl_scope());

  printf("OK\n");
}
