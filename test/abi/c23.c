#include "test.h"

_BitInt(333) bitint_fn11(unsigned char c, int m, _BitInt(300) h);
_BitInt(333) bitint_fn12(unsigned char c, int m, _BitInt(300) h) {
    return m * (h ^ c);
}

_BitInt(80) bitint_fn21(unsigned _BitInt(3) neg4,int food, _BitInt(80) v, int cat);
_BitInt(80) bitint_fn22(unsigned _BitInt(3) neg4,int food, _BitInt(80) v, int cat) {
    return cat + ((v >> 1) |
    (_BitInt(3))neg4 *-268435456) +
    (((unsigned _BitInt(48))food)<<16wb);
}

extern unsigned _BitInt(288) ext_bitint;

_BitInt(999) bitint() {
  {
#define LARGE_BITINT_FN(_fnv1, _fnv2) \
  (ext_bitint == _fnv1('2',0x157,_fnv2('4',0x157,0xd21e948f68a34c192f62ea79bc942dbe7ce182036415f56e34bac982aac4afe9fd9UWB)))

    ASSERT(1, LARGE_BITINT_FN(bitint_fn11, bitint_fn11));
    ASSERT(1, LARGE_BITINT_FN(bitint_fn11, bitint_fn12));
    ASSERT(1, LARGE_BITINT_FN(bitint_fn12, bitint_fn11));
    ASSERT(1, LARGE_BITINT_FN(bitint_fn12, bitint_fn12));
  }

  {
    ASSERT(1, bitint_fn21(4,0xF00d0Bab,0xbd95ffda00000001c001wb,0xca7) == 0xDecaffedF00d4BabeCa7uwb);
    ASSERT(1, bitint_fn22(4,0xF00d0Bab,0xbd95ffda00000001c001wb,0xca7) == 0xDecaffedF00d4BabeCa7uwb);
  }
  return -3;
}

int main(void) {
  ASSERT(-3, bitint());

  printf("OK\n");
}
