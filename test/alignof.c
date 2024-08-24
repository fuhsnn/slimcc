#include "test.h"

int _Alignas(512) g1;
int _Alignas(512) g2;
char g3;
int g4;
long g5;
char g6;

int main() {
  EASSERT(1, _Alignof(char));
  EASSERT(2, _Alignof(short));
  EASSERT(4, _Alignof(int));
  EASSERT(8, _Alignof(long));
  EASSERT(8, _Alignof(long long));
  EASSERT(1, _Alignof(char[3]));
  EASSERT(4, _Alignof(int[3]));
  EASSERT(1, _Alignof(struct {char a; char b;}[2]));
  EASSERT(8, _Alignof(struct {char a; long b;}[2]));

  // ASSERT(1, ({ _Alignas(char) char x, y; &y-&x; }));
  ASSERT(0, ({ _Alignas(long) char x, y; 7 & (intptr_t)&x; }));
  ASSERT(0, ({ _Alignas(32) char x, y; 31 & (intptr_t)&y; }));
  ASSERT(0, ({ _Alignas(32) int *x, *y; 31 & (intptr_t)&y; }));
  ASSERT(0, ({ struct { _Alignas(16) char x, y; } a; 15 & (intptr_t)&a.y; }));
  ASSERT(8, ({ struct T { _Alignas(8) char a; }; _Alignof(struct T); }));

  ASSERT(0, (long)(char *)&g1 % 512);
  ASSERT(0, (long)(char *)&g2 % 512);
  ASSERT(0, (long)(char *)&g4 % 4);
  ASSERT(0, (long)(char *)&g5 % 8);

  ASSERT(1, ({ char x; _Alignof(x); }));
  ASSERT(4, ({ int x; _Alignof(x); }));
  ASSERT(1, ({ char x; _Alignof x; }));
  ASSERT(4, ({ int x; _Alignof x; }));

  EASSERT(1, _Alignof(char) << 31 >> 31);
  EASSERT(1, _Alignof(char) << 63 >> 63);
  ASSERT(1, ({ char x; _Alignof(x) << 63 >> 63; }));

  ASSERT(0, ({ char x[16]; (unsigned long)&x % 16; }));
  ASSERT(0, ({ char x[17]; (unsigned long)&x % 16; }));
  ASSERT(0, ({ char x[100]; (unsigned long)&x % 16; }));
  ASSERT(0, ({ char x[101]; (unsigned long)&x % 16; }));

  printf("OK\n");
  return 0;
}
