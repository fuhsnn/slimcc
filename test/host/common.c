#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

void test_assert(int expected, int actual, char *code) {
  if (expected == actual) {
    printf("%s => %d\n", code, actual);
  } else {
    fprintf(stderr, "%s => %d expected but got %d\n", code, expected, actual);
    exit(1);
  }
}

static int static_fn() { return 5; }
int ext1 = 5;
int *ext2 = &ext1;
int ext3 = 7;
int ext_fn1(int x) { return x; }
int ext_fn2(int x) { return x; }
int common_ext2 = 3;
static int common_local;
_Thread_local int extern_tls;

int false_fn() { return 512; }
int true_fn() { return 513; }
int char_fn() { return (2<<8)+3; }
int short_fn() { return (2<<16)+5; }

int uchar_fn() { return (2<<10)-1-4; }
int ushort_fn() { return (2<<20)-1-7; }

int schar_fn() { return (2<<10)-1-4; }
int sshort_fn() { return (2<<20)-1-7; }

int add_all(int n, ...) {
  va_list ap;
  va_start(ap, n);

  int sum = 0;
  for (int i = 0; i < n; i++)
    sum += va_arg(ap, int);
  return sum;
}

float add_float(float x, float y) {
  return x + y;
}

double add_double(double x, double y) {
  return x + y;
}

int add10_int(int x1, int x2, int x3, int x4, int x5, int x6, int x7, int x8, int x9, int x10) {
  return x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10;
}

float add10_float(float x1, float x2, float x3, float x4, float x5, float x6, float x7, float x8, float x9, float x10) {
  return x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10;
}

double add10_double(double x1, double x2, double x3, double x4, double x5, double x6, double x7, double x8, double x9, double x10) {
  return x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10;
}

typedef struct { int a,b; short c; char d; } Ty4;
typedef struct { int a; float b; double c; } Ty5;
typedef struct { unsigned char a[3]; } Ty6;
typedef struct { long a, b, c; } Ty7;

int struct_test4(Ty4 x, int n) {
  switch (n) {
  case 0: return x.a;
  case 1: return x.b;
  case 2: return x.c;
  default: return x.d;
  }
}

int struct_test5(Ty5 x, int n) {
  switch (n) {
  case 0: return x.a;
  case 1: return x.b;
  default: return x.c;
  }
}

int struct_test6(Ty6 x, int n) {
  return x.a[n];
}

int struct_test7(Ty7 x, int n) {
  switch (n) {
  case 0: return x.a;
  case 1: return x.b;
  default: return x.c;
  }
}

Ty4 struct_test24(void) {
  return (Ty4){10, 20, 30, 40};
}

Ty5 struct_test25(void) {
  return (Ty5){10, 20, 30};
}

Ty6 struct_test26(void) {
  return (Ty6){10, 20, 30};
}

typedef struct { unsigned char a[10]; } Ty20;
typedef struct { unsigned char a[20]; } Ty21;

Ty20 struct_test27(void) {
  return (Ty20){10, 20, 30, 40, 50, 60, 70, 80, 90, 100};
}

Ty21 struct_test28(void) {
  return (Ty21){1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20};
}

typedef struct {
  char g;
} G;

typedef struct {
  float f;
} F;

typedef struct {
  char l;
  float d;
} A;


float struct_test101(
  G g0, G g1, G g2, G g3, G g4, G g5,
  F f0, F f1, F f2, F f3, F f4, F f5, F f6, F f7,
  G gs0, F fs0, G gs1, F fs1
) {
  return g0.g + g1.g + g2.g + g3.g + g4.g + g5.g +
  f0.f + f1.f + f2.f + f3.f + f4.f + f5.f + f6.f + f7.f +
  gs0.g + gs1.g + fs0.f + fs1.f;
}

float struct_test111(
  F f0, F f1, F f2, F f3, F f4, F f5, F f6, F f7,
  G g0, G g1, G g2, G g3, G g4, G g5,
  G gs0, F fs0, G gs1, F fs1
) {
  return
  g0.g + g1.g + g2.g + g3.g + g4.g + g5.g +
  f0.f + f1.f + f2.f + f3.f + f4.f + f5.f + f6.f + f7.f +
  gs0.g + gs1.g + fs0.f + fs1.f;
}


float struct_test121(
  A reg0, A reg1, A reg2, A reg3, A reg4, A reg5,
  A s0,
  F f6, F f7,
  A s1)
{
  return
  reg0.l + reg0.d + reg1.l + reg1.d + reg2.l + reg2.d +
  reg3.l + reg3.d + reg4.l + reg4.d + reg5.l + reg5.d +
  s0.l + s0.d + s1.l + s1.d + f6.f + f7.f;
}


typedef struct {
  char _Alignas(1024) c;
} Aligned1024;

int struct_test131(G g0,G g1,G g2,G g3,G g4,F f0,F f1,F f2,F f3,F f4,F f5, int i0, int i1, ... ) {
  va_list ap;
  va_start(ap, i1);
  long double ret = i0 + i1;
  ret += va_arg(ap, long double);
  ret += va_arg(ap, Aligned1024).c;
  ret += va_arg(ap, int);
  ret += va_arg(ap, double);
  ret += va_arg(ap, double);
  ret += va_arg(ap, double);
  ret += va_arg(ap, double);
  ret += va_arg(ap, Aligned1024).c;
  ret += va_arg(ap, long double);
  va_end(ap);
  return ret;
}

typedef struct {
  long i,j;
} DI;

int struct_test141(int cnt,...) {
  va_list ap;
  va_start(ap, cnt);
  long ret = 0;
  for (int i = 0; i < cnt; i++) {
    DI s = va_arg(ap, DI);
    ret += s.i + s.j;
  }
  va_end(ap);
  return ret;
}

typedef struct {
  double d,f;
} DD;

int struct_test151(int cnt,...) {
  va_list ap;
  va_start(ap, cnt);
  double ret = 0;
  for (int i = 0; i < cnt; i++) {
    DD s = va_arg(ap, DD);
    ret += s.d + s.f;
  }
  va_end(ap);
  return ret;
}

typedef struct {
    long i;
    double d;
} DM;
int struct_test161(int cnt,...) {
  va_list ap;
  va_start(ap, cnt);
  double ret = 0;
  for (int i = 0; i < cnt; i++) {
    DM s = va_arg(ap, DM);
    ret += s.d + s.i;
  }
  va_end(ap);
  return ret;
}
