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


int main(void) {
  bitextract();
  struct_init();


  printf("OK\n");

  return 0;
}
