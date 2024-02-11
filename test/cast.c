#include "test.h"

int main() {
  EASSERT(131585, (int)8590066177);
  EASSERT(513, (short)8590066177);
  EASSERT(1, (char)8590066177);
  EASSERT(1, (long)1);
  ASSERT(0, (long)&*(int *)0);
  ASSERT(513, ({ int x=512; *(char *)&x=1; x; }));
  ASSERT(5, ({ int x=5; long y=(long)&x; *(int*)y; }));

  (void)1;

  EASSERT(-1, (char)255);
  EASSERT(-1, (signed char)255);
  EASSERT(255, (unsigned char)255);
  EASSERT(-1, (short)65535);
  EASSERT(65535, (unsigned short)65535);
  EASSERT(-1, (int)0xffffffff);
  EASSERT(0xffffffff, (unsigned)0xffffffff);

  EASSERT(1, -1<1);
  EASSERT(0, -1<(unsigned)1);
  EASSERT(254, (char)127+(char)127);
  EASSERT(65534, (short)32767+(short)32767);
  EASSERT(-1, -1>>1);
  EASSERT(-1, (unsigned long)-1);
  EASSERT(2147483647, ((unsigned)-1)>>1);
  EASSERT(-50, (-100)/2);
  EASSERT(2147483598, ((unsigned)-100)/2);
  EASSERT(9223372036854775758, ((unsigned long)-100)/2);
  EASSERT(0, ((long)-1)/(unsigned)100);
  EASSERT(-2, (-100)%7);
  EASSERT(2, ((unsigned)-100)%7);
  EASSERT(6, ((unsigned long)-100)%9);

  EASSERT(65535, (int)(unsigned short)65535);
  ASSERT(65535, ({ unsigned short x = 65535; x; }));
  ASSERT(65535, ({ unsigned short x = 65535; (int)x; }));

  ASSERT(-1, ({ typedef short T; T x = 65535; (int)x; }));
  ASSERT(65535, ({ typedef unsigned short T; T x = 65535; (int)x; }));

  EASSERT(0, (_Bool)0.0);
  EASSERT(1, (_Bool)0.1);
  EASSERT(3, (char)3.0);
  EASSERT(1000, (short)1000.3);
  EASSERT(3, (int)3.99);
  EASSERT(2000000000000000, (long)2e15);
  EASSERT(3, (float)3.5);
  EASSERT(5, (double)(float)5.5);
  EASSERT(3, (float)3);
  EASSERT(3, (double)3);
  EASSERT(3, (float)3L);
  EASSERT(3, (double)3L);

  printf("OK\n");
  return 0;
}
