#include "test.h"

_BitInt(333) bitint_fn1(unsigned char c, int m, _BitInt(300) h);
_BitInt(80) bitint_fn2(unsigned _BitInt(3) neg4,int food, _BitInt(80) v, int cat);

extern unsigned _BitInt(288) ext_bitint;
_BitInt(999) bitint() {
  ASSERT(1, ext_bitint == bitint_fn1('2',0x157,bitint_fn1('4',0x157,0xd21e948f68a34c192f62ea79bc942dbe7ce182036415f56e34bac982aac4afe9fd9UWB)));

  ASSERT(1, bitint_fn2(4,0xF00d0Bab,0xbd95ffda00000001c001wb,0xca7) == 0xDecaffedF00d4BabeCa7uwb);
  return -3;
}

int va_callee(...);

int va_call() {
  unsigned _BitInt(15) pos = -1;

  _BitInt(7) neg = -42;

  _BitInt(100) large1 = 11;
  _BitInt(200) large2 = -22;
  _BitInt(300) large3 = -33;
  _BitInt(400) large4 = 44;

  return 8 == va_callee(pos, large1, 123, large2, neg, large3, 321, large4);
}

int main(void) {
  ASSERT(-3, bitint());

  ASSERT(1, va_call());

  printf("OK\n");
}
