#include "test.h"

int rec_idx = 0;
int rec[20];

void cln1(void *num) {
  rec[rec_idx++] = (*(long long*)num)++;
}

void cln2(void *num) {
  rec[rec_idx++] = 10 * (*(long long*)num)++;
}

#define A __attribute__((cleanup(cln1)))
#define B [[gnu::cleanup(cln2)]]
void test_decl(void) {
  rec_idx = 0;

  A long long p1 = 11;
  long A long p2 = 22;
  long long A p3 = 33, p4 = 44;
  long long p5 A = 55, p6 = 66;
  long long p7 = 77, A p8 = 88, pz = 999;
  long long p9 = 99, p11 A = 111;

  B long long p22 = 222;
  long long p33 B = 333, p44 = 444;
  long long p55 = 555, p66 B = 666;

  A long long p77 = 777, p88 B = 888;
}

void test_loop(void) {
  rec_idx = 0;

  for(B long long i = 0; i < 3; i++) {
    B long long j = 11;
    switch (i) {
    case 0:
      continue;
    case 1:
      B long long j = 22;
      continue;
    }
  }
}

void test_vla(int sz) {
  rec_idx = 0;

  long long i __attribute__((cleanup(cln1))) = 222;

  long long vla[sz * 2] __attribute__((cleanup(cln1)));
  vla[0] = 137;

  long long j __attribute__((cleanup(cln1))) = 444;

  long long vla2[sz * 3] __attribute__((cleanup(cln1)));
  vla2[0] = 248;

  long long k __attribute__((cleanup(cln1))) = 555;
  return;
}


void void_nortn(void) {
  long long j __attribute__((cleanup(cln1))) = 11;
}
void void_rtn(void) {
  long long j __attribute__((cleanup(cln1))) = 22;
  return;
}
int int_rtn(void) {
  long long j __attribute__((cleanup(cln1))) = 33;
  return j;
}

void float_cln(float* num) {
  rec[0] = (*num)++;
}
float float_rtn(void) {
  float f __attribute__((cleanup(float_cln))) = 44;
  return f;
}

void ldouble_cln(long double* num) {
  rec[0] = (*num)++;
}
long double ldouble_rtn(void) {
  long double f __attribute__((cleanup(ldouble_cln))) = 55;
  return f;
}

typedef struct {
  char c;
} Small;
void small_struct_cln(Small* s) {
  rec[0] = (s->c)++;
}
Small small_struct_rtn(void) {
  Small s __attribute__((cleanup(small_struct_cln))) = {66};
  return s;
}

typedef struct {
  int i[33];
  char c;
} Large;
void large_struct_cln(Large* s) {
  rec[0] = (s->c)++;
}
Large large_struct_rtn(void) {
  Large s __attribute__((cleanup(large_struct_cln))) = {.c = 77};
  return s;
}

struct S {
  int j,i;
};
void add(struct S* s) {
  ASSERT(11, s->i += 7);
}
void stmt_expr(void) {
  int i = ({
    struct S s __attribute__((cleanup(add))) = {3,4};
    s;
  }).i;
  ASSERT(4, i);
}

int main(void) {
  test_decl();
  ASSERT(888, rec[0]);
  ASSERT(777, rec[1]);
  ASSERT(6660, rec[2]);
  ASSERT(3330, rec[3]);
  ASSERT(2220, rec[4]);
  ASSERT(111, rec[5]);
  ASSERT(88, rec[6]);
  ASSERT(55, rec[7]);
  ASSERT(44, rec[8]);
  ASSERT(33, rec[9]);
  ASSERT(22, rec[10]);
  ASSERT(11, rec[11]);

  test_loop();
  ASSERT(110, rec[0]);
  ASSERT(220, rec[1]);
  ASSERT(110, rec[2]);
  ASSERT(110, rec[3]);
  ASSERT(30, rec[4]);

  test_vla(100);
  ASSERT(555, rec[0]);
  ASSERT(248, rec[1]);
  ASSERT(444, rec[2]);
  ASSERT(137, rec[3]);
  ASSERT(222, rec[4]);


  rec_idx = 0;
  void_nortn();
  ASSERT(11, rec[0]);
  void_rtn();
  ASSERT(22, rec[1]);
  ASSERT(33, int_rtn());
  ASSERT(33, rec[2]);

  ASSERT(44, (int)float_rtn());
  ASSERT(44, rec[0]);
  ASSERT(55, (int)ldouble_rtn());
  ASSERT(55, rec[0]);
  ASSERT(66, small_struct_rtn().c);
  ASSERT(66, rec[0]);
  ASSERT(77, large_struct_rtn().c);
  ASSERT(77, rec[0]);

  stmt_expr();

  printf("OK\n");
}
