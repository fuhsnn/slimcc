#include "test.h"
#include <stdint.h>

union {char a; int b;} g11[2] = {{1}, {3}};
union {int a[2];} g12[2] = {{{1, 2}}};
int *g28 = &g11[1].a;
union { struct { int a[3]; } a; } g30 = {{{1,2,3}}};
int *g31=g30.a.a;
union {int a[2];} g40[2] = {{1, 2}, 3, 4};
union {int a[2];} g41[2] = {1, 2, 3, 4};

int main() {
  ASSERT(1, ({ union {int i;} u = {.i=1,}; u.i; }));

  ASSERT(1, ({ union { int i,j; } u[] = {1,2,3,4}; u[0].i; }));
  ASSERT(1, ({ union { int i,j; } u[] = {1,2,3,4}; u[0].j; }));
  ASSERT(2, ({ union { int i,j; } u[] = {1,2,3,4}; u[1].i; }));
  ASSERT(2, ({ union { int i,j; } u[] = {1,2,3,4}; u[1].j; }));

  ASSERT(1, ({ typedef union { struct {int i,j;};} U; U u1={1,2}; U u2=u1; u2.i; }));
  ASSERT(2, ({ typedef union { struct {int i,j;};} U; U u1={1,2}; U u2=u1; u2.j; }));

  ASSERT(0, ({ typedef union { struct {int a,b;}; struct {int c,d;};} U; U u1={.d=1,}; U u2=u1; u2.a; }));
  ASSERT(1, ({ typedef union { struct {int a,b;}; struct {int c,d;};} U; U u1={.d=1,}; U u2=u1; u2.b; }));
  ASSERT(0, ({ typedef union { struct {int a,b;}; struct {int c,d;};} U; U u1={.b=1,}; U u2=u1; u2.c; }));
  ASSERT(1, ({ typedef union { struct {int a,b;}; struct {int c,d;};} U; U u1={.b=1,}; U u2=u1; u2.d; }));

  ASSERT(2, ({ union { struct {int i; int j;} t; int k;} s = {.t.i=1, .k=2,}; s.t.i; }));
  ASSERT(0, ({ union { struct {int i; int j;} t; int k;} s = {.t.i=1, .k=2,}; s.t.j; }));
  ASSERT(2, ({ union { struct {int i; int j;} t; int k;} s = {.t.i=1, .k=2,}; s.k; }));

  ASSERT(1, ({ union { struct {int a, b;} c;} s = {.c.a = 1,.c.b = 2,}; s.c.a; }));
  ASSERT(2, ({ union { struct {int a, b;} c;} s = {.c.a = 1,.c.b = 2,}; s.c.b; }));

  ASSERT(7, ({ union { struct {int a,b,c;};} s = {.a=7,5,3}; s.a; }));
  ASSERT(5, ({ union { struct {int a,b,c;};} s = {.a=7,5,3}; s.b; }));
  ASSERT(3, ({ union { struct {int a,b,c;};} s = {.a=7,5,3}; s.c; }));

  ASSERT(0, ({ union { struct {char a,b,c;};} s = {.b=7,5}; s.a; }));
  ASSERT(7, ({ union { struct {char a,b,c;};} s = {.b=7,5}; s.b; }));
  ASSERT(5, ({ union { struct {char a,b,c;};} s = {.b=7,5}; s.c; }));

  ASSERT(3, ({ union { struct {char a,b,c;};} s = {.b=7,5,.a=3,1}; s.a; }));
  ASSERT(1, ({ union { struct {char a,b,c;};} s = {.b=7,5,.a=3,1}; s.b; }));
  ASSERT(5, ({ union { struct {char a,b,c;};} s = {.b=7,5,.a=3,1}; s.c; }));

  ASSERT(3, ({ union { struct {int a,b,c;};} s = {3,.b=5,7}; s.a; }));
  ASSERT(5, ({ union { struct {int a,b,c;};} s = {3,.b=5,7}; s.b; }));
  ASSERT(7, ({ union { struct {int a,b,c;};} s = {3,.b=5,7}; s.c; }));

  ASSERT(3, ({ union { struct {int a,b,c;};} s = {3,5,.c=7}; s.a; }));
  ASSERT(5, ({ union { struct {int a,b,c;};} s = {3,5,.c=7}; s.b; }));
  ASSERT(7, ({ union { struct {int a,b,c;};} s = {3,5,.c=7}; s.c; }));

  ASSERT(1, ({ union {int a; int b; int c;} x={1,2,3}; x.a; }));
  ASSERT(1, ({ union {int a; int b; int c;} x={1,2,3}; x.b; }));
  ASSERT(1, ({ union {int a; int b; int c;} x={1,2,3}; x.c; }));
  ASSERT(1, ({ union {int a; int b; int c;} x={1}; x.a; }));
  ASSERT(1, ({ union {int a; int b; int c;} x={1}; x.b; }));
  ASSERT(1, ({ union {int a; int b; int c;} x={1}; x.c; }));

  ASSERT(1, ({ union {int a; int b;} x[2]={{1,2},{3,4}}; x[0].a; }));
  ASSERT(1, ({ union {int a; int b;} x[2]={{1,2},{3,4}}; x[0].b; }));
  ASSERT(3, ({ union {int a; int b;} x[2]={{1,2},{3,4}}; x[1].a; }));
  ASSERT(3, ({ union {int a; int b;} x[2]={{1,2},{3,4}}; x[1].b; }));

  ASSERT(2, ({ union {int a; int b;} x[2]={{1},{2}}; x[1].b; }));

  ASSERT(0, ({ union {int a; int b;} x={}; x.a; }));
  ASSERT(0, ({ union {int a; int b;} x={}; x.b; }));

  ASSERT(1, ({ typedef union {int a,b,c,d,e,f;} T; T x={1,2,3,4,5,6}; T y; y=x; y.e; }));
  ASSERT(1, ({ typedef union {int a,b;} T; T x={1,2}; T y, z; z=y=x; z.b; }));

  ASSERT(1, ({ typedef union {int a,b;} T; T x={1,2}; T y=x; y.a; }));

  ASSERT(1, g11[0].a);
  ASSERT(1, g11[0].b);
  ASSERT(3, g11[1].a);
  ASSERT(3, g11[1].b);

  ASSERT(1, g12[0].a[0]);
  ASSERT(2, g12[0].a[1]);
  ASSERT(0, g12[1].a[0]);
  ASSERT(0, g12[1].a[1]);

  ASSERT(3, *g28);

  ASSERT(1, g31[0]);
  ASSERT(2, g31[1]);
  ASSERT(3, g31[2]);

  ASSERT(1, g40[0].a[0]);
  ASSERT(2, g40[0].a[1]);
  ASSERT(3, g40[1].a[0]);
  ASSERT(4, g40[1].a[1]);

  ASSERT(1, g41[0].a[0]);
  ASSERT(2, g41[0].a[1]);
  ASSERT(3, g41[1].a[0]);
  ASSERT(4, g41[1].a[1]);

  ASSERT(0, ({ union {int a; int b;} x[2]={0,1,2,3}; x[0].a; }));
  ASSERT(1, ({ union {int a; int b;} x[2]={0,1,2,3}; x[1].a; }));

  ASSERT(1, ({ union {int a,b,c;} x={1,2,3,}; x.a; }));
  ASSERT(1, ({ union {int a; char b;} x={1,}; x.a; }));

  ASSERT(4, ({ union { int a,b; } x={1,.a=4}; x.a; }));
  ASSERT(4, ({ union { int a,b; } x={1,2,.b=3,.a=4}; x.b; }));

  ASSERT(1, ({ union { struct { int a,b; } c; } x={.c=1,2}; x.c.a; }));
  ASSERT(2, ({ union { struct { int a,b; } c; } x={.c=1,2}; x.c.b; }));

  ASSERT(0, ({ union { struct { int a,b; } c; } x={.c.b=1}; x.c.a; }));
  ASSERT(1, ({ union { struct { int a,b; } c; } x={.c.b=1}; x.c.b; }));

  ASSERT(1, ({ union { int a[2]; } x={.a=1,2}; x.a[0]; }));
  ASSERT(2, ({ union { int a[2]; } x={.a=1,2}; x.a[1]; }));

  ASSERT(0, ({ union { int a[2]; } x={.a[1]=1}; x.a[0]; }));
  ASSERT(1, ({ union { int a[2]; } x={.a[1]=1}; x.a[1]; }));

  ASSERT(3, ({ union { int a,b; } x[]={[1].b=1,2,[0]=3,}; x[0].a; }));
  ASSERT(3, ({ union { int a,b; } x[]={[1].b=1,2,[0]=3,}; x[0].b; }));
  ASSERT(1, ({ union { int a,b; } x[]={[1].b=1,2,[0]=3,}; x[1].a; }));
  ASSERT(1, ({ union { int a,b; } x[]={[1].b=1,2,[0]=3,}; x[1].b; }));
  ASSERT(2, ({ union { int a,b; } x[]={[1].b=1,2,[0]=3,}; x[2].a; }));
  ASSERT(2, ({ union { int a,b; } x[]={[1].b=1,2,[0]=3,}; x[2].b; }));

  ASSERT(3, ({ union { int a; struct {int b,c;}; } x[]={[1].c=1,2,[0]=3,}; x[0].a; }));
  ASSERT(0, ({ union { int a; struct {int b,c;}; } x[]={[1].c=1,2,[0]=3,}; x[0].c; }));
  ASSERT(0, ({ union { int a; struct {int b,c;}; } x[]={[1].c=1,2,[0]=3,}; x[1].a; }));
  ASSERT(1, ({ union { int a; struct {int b,c;}; } x[]={[1].c=1,2,[0]=3,}; x[1].c; }));
  ASSERT(2, ({ union { int a; struct {int b,c;}; } x[]={[1].c=1,2,[0]=3,}; x[2].a; }));
  ASSERT(0, ({ union { int a; struct {int b,c;}; } x[]={[1].c=1,2,[0]=3,}; x[2].c; }));

  ASSERT(1, ({ typedef union { int a,b; } T; T x={1,2}; T y[]={x}; y[0].a; }));
  ASSERT(1, ({ typedef union { int a,b; } T; T x={1,2}; T y[]={x}; y[0].b; }));
  ASSERT(1, ({ typedef union { int a,b; } T; T x={1}; T y[]={x, [2].b=3}; y[0].a; }));
  ASSERT(3, ({ typedef union { int a,b; } T; T x={1}; T y[]={x, [2].b=3}; y[2].b; }));

  ASSERT(5, ((union { int a,b,c; }){ .c=5 }).c);
  ASSERT(5, ((union { int a,b,c; }){ .c=5 }).a);

  ASSERT(3, ({ union { struct { int a; struct { int b, c; }; }; } x={.b=1,2,.a=3}; x.a; }));
  ASSERT(1, ({ union { struct { int a; struct { int b, c; }; }; } x={.b=1,2,.a=3}; x.b; }));
  ASSERT(2, ({ union { struct { int a; struct { int b, c; }; }; } x={.b=1,2,.a=3}; x.c; }));


  ASSERT(5, ({ union U { int8_t a; int64_t :37; }; sizeof(union U); }));
  ASSERT(1, ({ union U { int8_t a; int64_t :37; }; _Alignof(union U); }));

  ASSERT(6, ({ union U { int16_t a; int64_t :47; }; sizeof(union U); }));
  ASSERT(2, ({ union U { int16_t a; int64_t :47; }; _Alignof(union U); }));

  printf("OK\n");
  return 0;
}
