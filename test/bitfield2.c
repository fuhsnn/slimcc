#include "test.h"
struct M { int : 0; int f2 : 12; };

int bitextract(void){
struct Fields {
    _Bool    a :1;
    long long  :3;
    unsigned b :4;
    _Bool    c :1;
    unsigned d :3;
    unsigned e :2;
    long       :2;
};

  struct Fields s1 = {11,22,33,44,55,66,77};
  struct Fields s2 = {77,66,55,44,33,22,11};

  ASSERT(4, sizeof(struct Fields));
  ASSERT(4, _Alignof(struct Fields));
  ASSERT(0, memcmp(&(int){14689}, &s1, 2));
  ASSERT(0, memcmp(&(int){6433}, &s2, 2));
  ASSERT(1, s1.a);
  ASSERT(6, s1.b);
  ASSERT(1, s1.c);
  ASSERT(4, s1.d);
  ASSERT(3, s1.e);

  ASSERT(1, s2.a);
  ASSERT(2, s2.b);
  ASSERT(1, s2.c);
  ASSERT(4, s2.d);
  ASSERT(1, s2.e);
}

int struct_init(void) {
  struct M m = {11,22};
  ASSERT(11, ({ m.f2; }));

  ASSERT(11, ({ struct { int :1,a,:1,b,:1,:1,c,:1; } s = {11,22,33}; s.a; }));
  ASSERT(22, ({ struct { int :1,a,:1,b,:1,:1,c,:1; } s = {11,22,33}; s.b; }));
  ASSERT(33, ({ struct { int :1,a,:1,b,:1,:1,c,:1; } s = {11,22,33}; s.c; }));

  ASSERT(11, ({ struct { int a,:1,b,:1,:1,c,:1; } s = {.a=11,22,33}; s.a; }));
  ASSERT(22, ({ struct { int a,:1,b,:1,:1,c,:1; } s = {.a=11,22,33}; s.b; }));
  ASSERT(33, ({ struct { int a,:1,b,:1,:1,c,:1; } s = {.a=11,22,33}; s.c; }));

  ASSERT(11, ({ struct { struct { int a,:1,b,:1,:1,c,:1; }; } s = {.a=11,22,33}; s.a; }));
  ASSERT(22, ({ struct { struct { int a,:1,b; int;int :1,c,:1; }; } s = {.a=11,22,33}; s.b; }));
  ASSERT(33, ({ struct { struct { int a,:1,b,:1,:1,c,:1; }; } s = {.a=11,22,33}; s.c; }));

  ASSERT(22, ({ struct { struct { int a; int; int b; int; int; int c; int; }; } s = {.a=11,22,33}; s.b; }));

  ASSERT(33, ({ struct { int a; struct {int:1,:1;}; int b; } s = {11,{},33}; s.b; }));
  ASSERT(33, ({ struct { int a; struct {int:1,:1;}; int b; } s = {.a=11,{},33}; s.b; }));
  ASSERT(22, ({ struct { struct { }; int a; } s = {{},22,33}; s.a; }));
  ASSERT(22, ({ struct { struct { int :1,:1; }; int a; } s = {{},22,33}; s.a; }));

  ASSERT(22, ({ struct { int :1; int a; int :1; struct { int :1; int b; int:1; int c; }; } s = {11,22,33}; s.b; }));
  ASSERT(33, ({ struct { int :1; int a; int :1; struct { int :1; int b; int:1; int c; }; } s = {11,22,33}; s.c; }));
}

void union_init(void) {

  ASSERT(33, ({ union { int :1,:1; int a; } s = {33}; s.a; }));
  ASSERT(33, ({ struct { union { int :1,:1; }; int a;} s = {{23},33}; s.a;}));
  ASSERT(33, ({ struct { union { int :1,:1; }; int a;} s = {{23},33}; s.a;}));

  ASSERT(11, ({ struct { int a; union { int :1,:7; }; int b; } s = {11,{},33}; s.a; }));
  ASSERT(33, ({ struct { int a; union { int :1,:7; }; int b; } s = {11,{},33}; s.b; }));

  ASSERT(11, ({ union { struct { int a,:1,b,:1,:1,c,:1; }; } s = {.a=11,22,33}; s.a; }));
  ASSERT(22, ({ union { struct { int a,:1,b,:1,:1,c,:1; }; } s = {.a=11,22,33}; s.b; }));
  ASSERT(33, ({ union { struct { int a,:1,b,:1,:1,c,:1; }; } s = {.a=11,22,33}; s.c; }));


  ASSERT(22, ({ union { struct { int a, :1; int b, :1, :1; int c, :1; }; } s = {.a=11,22,33}; s.b; }));

  ASSERT(0, ({ union { union { }; int a; } s = {{}}; s.a; }));

  ASSERT(0, ({ union { struct { int :1,:1; }; int a; } s = {{}}; s.a; }));

  ASSERT(22, ({ struct { int a; union { int:1; int b; }; } s = {11,22}; s.b; }));
}

int assign_expr(void) {
    struct {
        int i : 2;
        _Bool b : 1;
        unsigned j : 2;
    } s = {.b = 1};
    int x = s.i = -5;
    int y = s.j = 5;
    ASSERT(-1, x);
    ASSERT(1, y);
    x = s.i += -5;
    y = s.j += 5;
    int z = s.b >>= 1;

    ASSERT(-2, x);
    ASSERT(2, y);
    ASSERT(0, (s.b >>= 1));
}

int large_field(void) {
  ASSERT(1, ({ struct { unsigned long long i: 56; } s = {.i = 0xFFFFFFFFFFFFFFFF }; s.i == 0xFFFFFFFFFFFFFF; }) );
}

struct {
    int b :3;
    int i, j;
} s = {.i = 2, 3};

int uninit_global(void) {
  ASSERT(2, s.i);
  ASSERT(3, s.j);
}

static int operand_promotion(void) {
  ASSERT(1, ({ struct { unsigned i:8; } s = {0}; s.i > -1; }) );
  ASSERT(1, ({ struct { unsigned i:8; } s = {0}; ~(0, s.i) < 0; }) );
  ASSERT(1, ({ struct { unsigned i:8; } s = {0}; ~({s.i;}) < 0; }) );
  ASSERT(1, ({ struct { unsigned i:8; } s = {0}; ~(s.i = 0) < 0; }) );
  ASSERT(1, ({ struct { unsigned i:8; } s = {0}; ~(1 ? s.i : s.i) < 0; }) );

  ASSERT(1, ({ struct { unsigned i:8; } s = {0}; ~(0, (1 ? s.i : s.i)) < 0; }) );
  ASSERT(1, ({ struct { unsigned i:8; } s = {0}; ~({(1 ? s.i : s.i);}) < 0; }) );
  ASSERT(1, ({ struct { unsigned i:8; } s = {0}; ~({s.i = 0;}) < 0; }) );
  ASSERT(1, ({ struct { unsigned i:8; } s = {0}; ~(1 ? ({(0,s.i);}) : (0,({s.i;}))) < 0; }) );
  ASSERT(1, ({ struct { unsigned i:8; } s = {0}; ~(1 ? (s.i = 0) : (0,({s.i;}))) < 0; }) );
  ASSERT(1, ({ struct { unsigned i:8; } s = {0}; ~(1 ? (s.i = 0) : ({(0,s.i);})) < 0; }) );
}

int main(void) {
  bitextract();
  struct_init();
  union_init();
  assign_expr();
  large_field();
  uninit_global();
  operand_promotion();

  printf("OK\n");

  return 0;
}
