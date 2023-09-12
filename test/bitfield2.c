#include "test.h"
struct M { int : 0; int f2 : 12; };

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
  struct_init();


  printf("OK\n");

  return 0;
}
