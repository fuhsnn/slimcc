#include "test.h"

#define A1 __attribute__((aligned(1024)))
#define A2 __attribute__((aligned(4096)))

#define B1 [[gnu::aligned(1024)]]

#define P __attribute__((packed))

A1 struct { int i; } g0, g1;
struct A1 { int i; } g2, g3;
struct { int i; } A1 g4, g5;
struct { int i; } g6, A1 g7;
struct { int i; } g8, g9 A1;

B1 struct { int i; } g20, g21;
struct B1 { int i; } g22, g23;
struct { int i; } g28, g29 B1;

struct { int i; } *A1 g31;

struct { int i; } g32 [1]A1;
struct { int i; } g33 B1[1];

int main(int argc, char **argv) {
  ASSERT(0, (long)&g1 & 1023);
  ASSERT(0, (long)&g3 & 1023);
  ASSERT(0, (long)&g5 & 1023);
  ASSERT(0, (long)&g7 & 1023);
  ASSERT(0, (long)&g9 & 1023);

  ASSERT(0, (long)&g21 & 1023);
  ASSERT(0, (long)&g23 & 1023);
  ASSERT(0, (long)&g29 & 1023);

  ASSERT(0, (long)&g31 & 1023);

  ASSERT(0, (long)&g32[0] & 1023);
  ASSERT(0, (long)&g33[0] & 1023);
  ASSERT(4, (long)&g32[1] & 1023);
  ASSERT(4, (long)&g33[1] & 1023);

  ASSERT(0, ({ A1 struct { int i; } x, y; (long)&y & 1023; }) );
  ASSERT(0, ({ struct A1 { int i; } x, y; (long)&y & 1023; }) );
  ASSERT(0, ({ struct { int i; } A1 x, y; (long)&y & 1023; }) );
  ASSERT(0, ({ struct { int i; } x, A1 y; (long)&y & 1023; }) );
  ASSERT(0, ({ struct { int i; } x, y A1; (long)&y & 1023; }) );
  ASSERT(0, ({ struct { int i; } A2 x, y A1; (long)&y & 4095; }) );
  ASSERT(0, ({ struct { int i; } A1 x, y A2; (long)&y & 4095; }) );

  ASSERT(0, ({ struct { A1 int i, j; } x; (long)&x.j & 1023; }) );
  ASSERT(0, ({ struct { int A1 i, j; } x; (long)&x.j & 1023; }) );
#ifdef NOTGCC
  ASSERT(0, ({ struct { int i, A1 j; } x; (long)&x.j & 1023; }) );
#endif
  ASSERT(0, ({ struct { int i, j A1; } x; (long)&x.j & 1023; }) );

  // ASSERT(0, ({ struct { A1 struct { int i; }; } x, y; (long)&y & 1023; }) ); // clang behaviour
  ASSERT(0, ({ struct { struct A1 { int i; }; } x, y; (long)&y & 1023; }) );
  ASSERT(0, ({ struct { struct { int i; } A1; } x, y; (long)&y & 1023; }) );

  ASSERT(0, ({ B1 struct { int i; } x, y; (long)&y & 1023; }) );
  ASSERT(0, ({ struct B1 { int i; } x, y; (long)&y & 1023; }) );
  ASSERT(0, ({ struct { int i; } x, y B1; (long)&y & 1023; }) );

  ASSERT(0, ({ struct { B1 int i, j; } x; (long)&x.j & 1023; }) );
  ASSERT(0, ({ struct { int i, j B1; } x; (long)&x.j & 1023; }) );

  ASSERT(0, ({ A1 static char x; (long)&x & 1023; }) );
  ASSERT(0, ({ static A1 char x; (long)&x & 1023; }) );
  ASSERT(0, ({ static char A1 x; (long)&x & 1023; }) );
  ASSERT(0, ({ static char x A1; (long)&x & 1023; }) );

  ASSERT(0, ({ A1 char x[argc]; (long)&x[0] & 1023; }) );
  ASSERT(0, ({ char A1 x[argc]; (long)&x[0] & 1023; }) );
  ASSERT(0, ({ char x[argc] A1; (long)&x[0] & 1023; }) );

  ASSERT(1024, ({ A1 typedef char c; _Alignof(c); }) );
  ASSERT(1024, ({ typedef A1 char c; _Alignof(c); }) );
  ASSERT(1024, ({ typedef char A1 c; _Alignof(c); }) );
  ASSERT(1024, ({ typedef char c A1; _Alignof(c); }) );

  ASSERT(16, ({ [[gnu::aligned]] typedef char c; _Alignof(c); }) );
  ASSERT(16, ({ typedef char c [[gnu::aligned]]; _Alignof(c); }) );

  ASSERT(1, _Alignof(char));

  ASSERT(0, ({ A1 typedef struct { int i; } S; S x; (long)&x & 1023; }) );
  ASSERT(0, ({ typedef A1 struct { int i; } S; S x; (long)&x & 1023; }) );
  ASSERT(0, ({ typedef struct A1 { int i; } S; S x; (long)&x & 1023; }) );
  ASSERT(0, ({ typedef struct { int i; } A1 S; S x; (long)&x & 1023; }) );
  ASSERT(0, ({ typedef struct { int i; } S A1; S x; (long)&x & 1023; }) );
  ASSERT(0, ({ typedef struct { int i; } S; A1 S x; (long)&x & 1023; }) );
  ASSERT(0, ({ typedef struct { int i; } S; S A1 x; (long)&x & 1023; }) );
  ASSERT(0, ({ typedef struct { int i; } S; S x A1; (long)&x & 1023; }) );

  ASSERT(8, ({ struct S { char c; void*p; }; offsetof(struct S, p);}) );
  ASSERT(16, ({ struct S { char c; void*p; }; sizeof(struct S);}) );
  ASSERT(1, ({ struct S { char c; void*p; } P; offsetof(struct S, p);}) );
  ASSERT(9, ({ struct S { char c; void*p; } P; sizeof(struct S);}) );

  ASSERT(8, ({ struct S { char c; void*p [[gnu::aligned(2)]]; }; offsetof(struct S, p);}) );
  ASSERT(16, ({ struct S { char c; void*p [[gnu::aligned(2)]]; }; sizeof(struct S);}) );
  ASSERT(2, ({ struct S { char c; void*p [[gnu::aligned(2)]]; } P; offsetof(struct S, p);}) );
  ASSERT(10, ({ struct S { char c; void*p [[gnu::aligned(2)]]; } P; sizeof(struct S);}) );

  ASSERT(8, ({ struct S { char c; void*p [[gnu::aligned(4)]]; }; offsetof(struct S, p);}) );
  ASSERT(16, ({ struct S { char c; void*p [[gnu::aligned(4)]]; }; sizeof(struct S);}) );
  ASSERT(4, ({ struct S { char c; void*p [[gnu::aligned(4)]]; } P; offsetof(struct S, p);}) );
  ASSERT(12, ({ struct S { char c; void*p [[gnu::aligned(4)]]; } P; sizeof(struct S);}) );

  ASSERT(8, ({ struct S { char c; void*p [[gnu::aligned(8)]]; }; offsetof(struct S, p);}) );
  ASSERT(16, ({ struct S { char c; void*p [[gnu::aligned(8)]]; }; sizeof(struct S);}) );
  ASSERT(8, ({ struct S { char c; void*p [[gnu::aligned(8)]]; } P; offsetof(struct S, p);}) );
  ASSERT(16, ({ struct S { char c; void*p [[gnu::aligned(8)]]; } P; sizeof(struct S);}) );

  ASSERT(16, ({ struct S { char c; void*p [[gnu::aligned(16)]]; }; offsetof(struct S, p);}) );
  ASSERT(32, ({ struct S { char c; void*p [[gnu::aligned(16)]]; }; sizeof(struct S);}) );
  ASSERT(16, ({ struct S { char c; void*p [[gnu::aligned(16)]]; } P; offsetof(struct S, p);}) );
  ASSERT(32, ({ struct S { char c; void*p [[gnu::aligned(16)]]; } P; sizeof(struct S);}) );

  ASSERT(32, ({ struct S { char c; void*p [[gnu::aligned(32)]]; }; offsetof(struct S, p);}) );
  ASSERT(64, ({ struct S { char c; void*p [[gnu::aligned(32)]]; }; sizeof(struct S);}) );
  ASSERT(32, ({ struct S { char c; void*p [[gnu::aligned(32)]]; } P; offsetof(struct S, p);}) );
  ASSERT(64, ({ struct S { char c; void*p [[gnu::aligned(32)]]; } P; sizeof(struct S);}) );

  ASSERT(8192, ({ typedef struct { struct { long m3 A2; }; A1 short m4; } T; sizeof(T); }));
  ASSERT(4096, ({ typedef struct { struct { long m3 A2; }; A1 short m4; } T; offsetof(T, m4); }));

  ASSERT(32, ({ typedef struct { char m1; union { long double m6 __attribute__((aligned(4))); long m7; }; } T; sizeof(T); }));
  ASSERT(16, ({ typedef struct { char m1; union { long double m6 __attribute__((aligned(4))); long m7; }; } T; offsetof(T, m7); }));
  ASSERT(20, ({ typedef struct { char m1; union { long double m6 __attribute__((aligned(4))); long m7; } P; } T; sizeof(T); }));
  ASSERT(4,  ({ typedef struct { char m1; union { long double m6 __attribute__((aligned(4))); long m7; } P; } T; offsetof(T, m7); }));
  ASSERT(1026, ({ typedef struct { short : 2, : 4  A1; char c; } T; sizeof(T); }));
  ASSERT(1025, ({ typedef struct { short : 2, : 4  A1; char c; } T; offsetof(T, c); }));

  ASSERT(17, ({ typedef struct { char : 6, : 0 __attribute__((aligned)), m6; } T; sizeof(T); }));
  ASSERT(16, ({ typedef struct { char : 6, : 0 __attribute__((aligned)), m6; } T; offsetof(T, m6); }));
  ASSERT(8, ({struct { long m1:5 __attribute__((aligned(4))); } T; sizeof(T); }));

  ASSERT(16, ({ typedef struct { short s; long m2 : 48 __attribute__((aligned(4))); } T; sizeof(T); }));
#ifdef NOTCLANG
  ASSERT(14, ({ typedef struct { short s; long m2 : 48 __attribute__((aligned(4))); char c; } T; offsetof(T,c); }));
#endif
  ASSERT(8, ({ typedef struct { char m1; int : 26 __attribute__((aligned(2))); } T; sizeof(T); }));
  ASSERT(8, ({ typedef struct { char m1; int : 26 __attribute__((aligned(2))); char c; } T; offsetof(T,c); }));

  ASSERT(16,  ({ struct { int m1 __attribute__((aligned));} P t; sizeof(t); }));
  ASSERT(1028,  ({ typedef struct { struct { char c; } A1; int m4; } P T; sizeof (T); }));
  ASSERT(1024,  ({ typedef struct { struct { char c; } A1; int m4; } P T; offsetof (T, m4); }));

  ASSERT(1025, ({ typedef struct { struct { char m2; union { int m4; } A1; } P; } T; sizeof(T);}));
  ASSERT(1,    ({ typedef struct { struct { char m2; union { int m4; } A1; } P; } T; offsetof(T,m4);}));

  ASSERT(1024, ({ typedef struct { A1 struct { char c; } j; char m6; } T; sizeof(T); }));
  ASSERT(1024, ({ typedef struct { A1 struct { char c; } j; } T; sizeof(T); }));
//  ASSERT(1024,    ({ typedef struct { A1 struct { char c; }; } T; sizeof(T); })); // clang behaviour
#ifdef NOTCLANG
  ASSERT(1,    ({ typedef struct { A1 struct { char c; }; } T; sizeof(T); })); // gcc behaviour
#endif

  ASSERT(3,    ({ A1 typedef struct { struct { char c; } j, k; char m6; } T; sizeof(T); }));
  ASSERT(3,    ({ typedef A1 struct { struct { char c; } j, k; char m6; } T; sizeof(T); }));
  ASSERT(1024, ({ typedef struct A1 { struct { char c; } j, k; char m6; } T; sizeof(T); }));
  ASSERT(2048, ({ typedef struct { A1 struct { char c; } j, k; char m6; } T; sizeof(T); }));
  ASSERT(3072, ({ typedef struct { struct A1 { char c; } j, k; char m6; } T; sizeof(T); }));
  ASSERT(3072, ({ typedef struct { struct { A1 char c; } j, k; char m6; } T; sizeof(T); }));
  ASSERT(3072, ({ typedef struct { struct { char A1 c; } j, k; char m6; } T; sizeof(T); }));
  ASSERT(3072, ({ typedef struct { struct { char c A1; } j, k; char m6; } T; sizeof(T); }));
  ASSERT(3072, ({ typedef struct { struct { char c; } A1 j, k; char m6; } T; sizeof(T); }));
  ASSERT(1024, ({ typedef struct { struct { char c; } j A1, k; char m6; } T; sizeof(T); }));
#ifdef NOTGCC
  ASSERT(2048, ({ typedef struct { struct { char c; } j, A1 k; char m6; } T; sizeof(T); }));
#endif
  ASSERT(2048, ({ typedef struct { struct { char c; } j, k A1; char m6; } T; sizeof(T); }));
  ASSERT(2048, ({ typedef struct { struct { char c; } j, k; A1 char m6; } T; sizeof(T); }));
  ASSERT(2048, ({ typedef struct { struct { char c; } j, k; char A1 m6; } T; sizeof(T); }));
  ASSERT(2048, ({ typedef struct { struct { char c; } j, k; char m6 A1; } T; sizeof(T); }));
  ASSERT(1024, ({ typedef struct { struct { char c; } j, k; char m6; } A1 T; sizeof(T); }));
  ASSERT(3,    ({ typedef struct { struct { char c; } j, k; char m6; } T A1; sizeof(T); }));


  ASSERT(1,    ({ A1 typedef struct { struct { char c; } j, k; char m6; } T; offsetof(T, k); }));
  ASSERT(1,    ({ typedef A1 struct { struct { char c; } j, k; char m6; } T; offsetof(T, k); }));
  ASSERT(1,    ({ typedef struct A1 { struct { char c; } j, k; char m6; } T; offsetof(T, k); }));
  ASSERT(1024, ({ typedef struct { A1 struct { char c; } j, k; char m6; } T; offsetof(T, k); }));
  ASSERT(1024, ({ typedef struct { struct A1 { char c; } j, k; char m6; } T; offsetof(T, k); }));
  ASSERT(1024, ({ typedef struct { struct { A1 char c; } j, k; char m6; } T; offsetof(T, k); }));
  ASSERT(1024, ({ typedef struct { struct { char A1 c; } j, k; char m6; } T; offsetof(T, k); }));
  ASSERT(1024, ({ typedef struct { struct { char c A1; } j, k; char m6; } T; offsetof(T, k); }));
  ASSERT(1024, ({ typedef struct { struct { char c; } A1 j, k; char m6; } T; offsetof(T, k); }));
  ASSERT(1,    ({ typedef struct { struct { char c; } j A1, k; char m6; } T; offsetof(T, k); }));
#ifdef NOTGCC
  ASSERT(1024, ({ typedef struct { struct { char c; } j, A1 k; char m6; } T; offsetof(T, k); }));
#endif
  ASSERT(1024, ({ typedef struct { struct { char c; } j, k A1; char m6; } T; offsetof(T, k); }));
  ASSERT(1,    ({ typedef struct { struct { char c; } j, k; A1 char m6; } T; offsetof(T, k); }));
  ASSERT(1,    ({ typedef struct { struct { char c; } j, k; char A1 m6; } T; offsetof(T, k); }));
  ASSERT(1,    ({ typedef struct { struct { char c; } j, k; char m6 A1; } T; offsetof(T, k); }));
  ASSERT(1,    ({ typedef struct { struct { char c; } j, k; char m6; } A1 T; offsetof(T, k); }));
  ASSERT(1,    ({ typedef struct { struct { char c; } j, k; char m6; } T A1; offsetof(T, k); }));

  ASSERT(2,    ({ A1 typedef struct { struct { char c; } j, k; char m6; } T; offsetof(T,m6); }));
  ASSERT(2,    ({ typedef A1 struct { struct { char c; } j, k; char m6; } T; offsetof(T,m6); }));
  ASSERT(2,    ({ typedef struct A1 { struct { char c; } j, k; char m6; } T; offsetof(T,m6); }));
  ASSERT(1025, ({ typedef struct { A1 struct { char c; } j, k; char m6; } T; offsetof(T,m6); }));
  ASSERT(2048, ({ typedef struct { struct A1 { char c; } j, k; char m6; } T; offsetof(T,m6); }));
  ASSERT(2048, ({ typedef struct { struct { A1 char c; } j, k; char m6; } T; offsetof(T,m6); }));
  ASSERT(2048, ({ typedef struct { struct { char A1 c; } j, k; char m6; } T; offsetof(T,m6); }));
  ASSERT(2048, ({ typedef struct { struct { char c A1; } j, k; char m6; } T; offsetof(T,m6); }));
  ASSERT(2048, ({ typedef struct { struct { char c; } A1 j, k; char m6; } T; offsetof(T,m6); }));
  ASSERT(2,    ({ typedef struct { struct { char c; } j A1, k; char m6; } T; offsetof(T,m6); }));
#ifdef NOTGCC
  ASSERT(1025, ({ typedef struct { struct { char c; } j, A1 k; char m6; } T; offsetof(T,m6); }));
#endif
  ASSERT(1025, ({ typedef struct { struct { char c; } j, k A1; char m6; } T; offsetof(T,m6); }));
  ASSERT(1024, ({ typedef struct { struct { char c; } j, k; A1 char m6; } T; offsetof(T,m6); }));
  ASSERT(1024, ({ typedef struct { struct { char c; } j, k; char A1 m6; } T; offsetof(T,m6); }));
  ASSERT(1024, ({ typedef struct { struct { char c; } j, k; char m6 A1; } T; offsetof(T,m6); }));
  ASSERT(2,    ({ typedef struct { struct { char c; } j, k; char m6; } A1 T; offsetof(T,m6); }));
  ASSERT(2,    ({ typedef struct { struct { char c; } j, k; char m6; } T A1; offsetof(T,m6); }));

#ifdef NOTCLANG
  ASSERT(2, ({ _Alignas(__attribute__((aligned(2))) long) char v; _Alignof(v); }));
#endif

  {
    typedef const int Ic;
    typedef A1 Ic Ica;
    typedef A1 int Ia;
    typedef volatile Ia Iav;
    Ica v1;
    Iav v2;
    SASSERT(alignof(Ica) == 1024);
    SASSERT(alignof(Iav) == 1024);
    SASSERT(alignof(v1) == 1024);
    SASSERT(alignof(v2) == 1024);
    SASSERT(_Generic(Ica, int const: 1));
    SASSERT(_Generic(Iav, int volatile: 1));
    SASSERT(_Generic(&v1, int const*: 1));
    SASSERT(_Generic(&v2, int volatile*: 1));
    ASSERT(0, 1023 & (int)&v1);
    ASSERT(0, 1023 & (int)&v2);
  }

  {
    typedef char T1;
    typedef char T1 A2;

    typedef char T2 A2;
    typedef char T2;

    typedef char T3 A1;
    typedef char T3 A2;

    typedef char T4 A2;
    typedef char T4 A1;

    SASSERT(alignof(T1) == 4096);
    SASSERT(alignof(T2) == 4096);
    SASSERT(alignof(T3) == 4096);
    SASSERT(alignof(T4) == 4096);
  }
  {
    typedef char T1 A1; T1 fn1(void);
    typedef char T2; A1 T2 fn2(void);

    SASSERT(alignof(fn1) == alignof(main));
    SASSERT(alignof(fn2) == 1024);
    SASSERT(alignof(fn1()) == 1024);
    SASSERT(alignof(fn2()) == alignof(char));
  }
  {
    typedef char T1 A1; A2 T1 i1;
    typedef char T2 A2; A1 T2 i2;

    SASSERT(alignof(i1) == 4096);
    SASSERT(alignof(i2) == 1024);
    SASSERT(alignof(typeof(i1)) == 1024);
    SASSERT(alignof(typeof(i2)) == 4096);
    ASSERT(0, 4095 & (int)&i1);
    ASSERT(0, 1023 & (int)&i2);
  }

  printf("OK\n");
}
