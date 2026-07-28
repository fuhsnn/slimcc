#include "test.h"

typedef struct {
  long i[4];
} S1;

S1 gen_S1(int i) { S1 s = {i,i+1,i+2,i+3}; return s; }

void *pass(void *ptr){ return ptr; }

typedef struct {
  int64_t arr[10];
} S2 __attribute__((aligned(256)));

static int64_t *fill10(int ofs, int64_t *p) {
  for (int i = 0; i < 10; i++)
    p[i] = ofs + i;
  return p;
}

static int test10(int ofs, int64_t *p) {
  for (int i = 0; i < 10; i++)
    if (p[i] != ofs + i)
      return 0;
  return 1;
}

static void *pass_mixed(S2 s1, int64_t *p1, S2 s2, int64_t *p2) {
  ASSERT(1, test10(10, s1.arr));
  ASSERT(1, test10(20, p1));
  ASSERT(1, test10(30, s2.arr));
  ASSERT(1, test10(40, p2));
}

static void *pass_aligned(S2 s1) {
  ASSERT(1, test10(10, s1.arr));
}


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

static int alloca_callarg() {
  void *alloca_p1 = pass(alloca(3));
  void *alloca_p2 = pass(alloca(3));
  ASSERT(1, alloca_p1 != alloca_p2);

  void *alloca_p3 = pass(({ alloca(3); }));
  ASSERT(1, alloca_p2 != alloca_p3);

  void *alloca_p4 = pass(({ alloca(3); }));
  ASSERT(1, alloca_p3 != alloca_p4);

  {
    void *p1, *p2, *p3;
    p2 = (p1 = fill10(11, alloca(80)), &pass)(fill10(22, alloca(80)));
    p3 = fill10(33, alloca(80));
    ASSERT(1, test10(11, p1));
    ASSERT(1, test10(22, p2));
    ASSERT(1, test10(33, p3));

    void *p4, *p5;
    S2 s1, s2;
    fill10(10, s1.arr);
    (p4 = fill10(44, alloca(80)), pass_aligned)(s1);
    p5 = fill10(55, alloca(80));
    ASSERT(1, test10(44, p4));
    ASSERT(1, test10(55, p5));
  }

  {
    void *p1, *p2;
    S2 s1, s2;
    fill10(10, s1.arr);
    fill10(30, s2.arr);
    (p1 = fill10(50, alloca(80)), pass_mixed)(
      s1,
      pass(fill10(20, pass(alloca(80)))),
      (struct {S2 s;}){s2}.s,
      pass(fill10(40, pass(alloca(80))))
    );
    p2 = fill10(60, alloca(80));

    ASSERT(1, test10(50, p1));
    ASSERT(1, test10(60, p2));
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

  void *complit_p1 = pass(&(S1[]){0});
  void *complit_p2 = pass(&(S1[]){0});
  void *complit_p3 = pass(pass(&(S1[]){0}));
  void *complit_p4 = pass(pass(&(S1[]){0}));

  ASSERT(1, complit_p1 != complit_p2);
  ASSERT(1, complit_p2 != complit_p3);
  ASSERT(1, complit_p3 != complit_p4);

  va_fn(0, (M){55,66});

  ASSERT(1, decl_scope());
  ASSERT(1, alloca_callarg());

  printf("OK\n");
}
