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

void signed_large_field(void) {
  struct {
    long long i:4, j:33, k:27;
  } s;
  s.k = -1234;
  s.i = -1;
  char buf[32];
  snprintf(buf, 32, "%d, %lld, %d", s.i, s.j = -54321, s.k);
  ASSERT(0, strcmp("-1, -54321, -1234", buf));
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
  ASSERT(0, ({ struct { unsigned i:8; } s = {0}; ((unsigned)s.i - 1) < 0; }) );
  ASSERT(1, ({ struct { unsigned i:8; } s = {0}; ~(0, s.i) < 0; }) );
  ASSERT(1, ({ struct { unsigned i:8; } s = {0}; ~({s.i;}) < 0; }) );
  ASSERT(1, ({ struct { unsigned i:8; } s = {0}; ~(s.i = 0) < 0; }) );
  ASSERT(1, ({ struct { unsigned i:8; } s = {0}; ~(s.i += 0) < 0; }) );
  ASSERT(1, ({ struct { unsigned i:8; } s = {0}; ~(s.i++) < 0; }) );
  ASSERT(1, ({ struct { unsigned i:8; } s = {0}; ~(1 ? s.i : s.i) < 0; }) );

  ASSERT(1, ({ struct { unsigned i:8; } s = {0}; ~(0, (1 ? s.i : s.i)) < 0; }) );
  ASSERT(1, ({ struct { unsigned i:8; } s = {0}; ~({(1 ? s.i : s.i);}) < 0; }) );
  ASSERT(1, ({ struct { unsigned i:8; } s = {0}; ~({s.i = 0;}) < 0; }) );
  ASSERT(1, ({ struct { unsigned i:8; } s = {0}; ~(1 ? ({(0,s.i);}) : (0,({s.i;}))) < 0; }) );
  ASSERT(1, ({ struct { unsigned i:8; } s = {0}; ~(1 ? (s.i = 0) : (0,({s.i;}))) < 0; }) );
  ASSERT(1, ({ struct { unsigned i:8; } s = {0}; ~(1 ? (s.i = 0) : ({(0,s.i);})) < 0; }) );
}

static void aligned_store(void) {
  uint32_t v = 0x10100;
  struct {
    unsigned u : 16, u2 : 15;
  } s = {0};
  ASSERT(1, ((s.u = v) == 0x100));
  ASSERT(1, (s.u == 0x100));
  ASSERT(1, (s.u2 == 0));
}

static int packed_fields() {

#define PACKED_CHK2(T,N,va,vb,vc) do { \
    { T N = {va,vb,vc}; ASSERT(1, N.a == va && N.b == vb && N.c == vc); } \
    { constexpr T N = {va,vb,vc}; SASSERT(N.a == va && N.b == vb && N.c == vc); } \
  } while(0)

#define PACKED_CHK(T,N,S) do {  \
    SASSERT(sizeof(T) == S);  \
    PACKED_CHK2(T,N, -1,0,0); \
    PACKED_CHK2(T,N, 0,-1,0); \
    PACKED_CHK2(T,N, 0,0,-1); \
  } while(0)

  {
    struct S {
      char a:7;
      long b:63;
      char c:2;
    } __attribute__((packed));
    PACKED_CHK(struct S, packed1, 9);
  }
  {
    struct S {
      int a:7, b:22, c:3;
    } __attribute__((packed));
    PACKED_CHK(struct S, packed2, 4);
  }
  {
    struct S {
      int a : 3, b : 3, c : 3;
    } __attribute__((packed));
    PACKED_CHK(struct S, packed3, 2);
  }
  {
    struct S {
      int a : 17, b : 4, c : 3;
    } __attribute__((packed));
    PACKED_CHK(struct S, packed4, 3);
  }
  {
    struct S {
      int a : 20;
      int b;
      int c : 22;
    } __attribute__((packed));
    PACKED_CHK(struct S, packed5, 10);
  }
  {
    struct S {
      int a : 30;
      int b : 27;
      int c : 3;
    } __attribute__((packed));
    PACKED_CHK(struct S, packed6, 8);
  }
  {
    struct S {
      short a;
      __attribute__((packed))int b : 24;
      char c;
    };
    PACKED_CHK(struct S, packed7, 6);
  }
  return 1;
}

int main(void) {
  bitextract();
  struct_init();
  union_init();
  assign_expr();
  large_field();
  signed_large_field();
  uninit_global();
  operand_promotion();
  aligned_store();
  ASSERT(1, packed_fields());

  printf("OK\n");

  return 0;
}
