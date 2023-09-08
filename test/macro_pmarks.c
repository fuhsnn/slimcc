#include "test.h"

#define HASH(x) #x
#define STR(x) HASH(x)
#define STR_STUFF(X,...) #__VA_OPT__(X X ## X __VA_ARGS__ X ## ## X X)

int main(void) {

#define M(x,y,z) x ## ## y ## ## z
  ASSERT(0, strcmp("3", STR(M(,,3))));

  ASSERT(0, strcmp("3", STR_STUFF(,M(,,3))));

#define M1(X) a X ## X ## b
  ASSERT(0, strcmp("a b", STR(M1())));

#define M2(X, ...) __VA_OPT__(a X) ## b
  ASSERT(0, strcmp("a b", STR(M2(,.))));

#define M3(X, ...) a ## __VA_OPT__(X b)
  ASSERT(0, strcmp("a b", STR(M3(,.))));

#define M4(X, ...) __VA_OPT__(a X ## X) ## b
  ASSERT(0, strcmp("a b", STR(M4(,.))));

#define M5(X, ...) a ## __VA_OPT__(X ## X b)
  ASSERT(0, strcmp("a b", STR(M5(,.))));

  printf("OK\n");
}
