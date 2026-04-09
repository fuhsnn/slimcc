#include <stdarg.h>

_BitInt(333) bitint_fn1(unsigned char c, int m, _BitInt(300) h) {
    return m * (h ^ c);
}

_BitInt(80) bitint_fn2(unsigned _BitInt(3) neg4,int food, _BitInt(80) v, int cat) {
    return cat + ((v >> 1) |
    (_BitInt(3))neg4 *-268435456) +
    (((unsigned _BitInt(48))food)<<16wb);
}

unsigned _BitInt(288) ext_bitint =  0x17933d7af45d70def423a316f14117df272cd0fd6b85f0f7c9bf6c5196b3160d02f6c1dfUWB;

int va_callee(...) {
  va_list ap;
  va_start(ap);
  int i = 0;
  i += 32767 == va_arg(ap, unsigned _BitInt(15));
  i += 11 == va_arg(ap, _BitInt(100));
  i += 123 == va_arg(ap, int);
  i += -22 == va_arg(ap, _BitInt(200));
  i += -42 == va_arg(ap, _BitInt(7));
  i += -33 == va_arg(ap, _BitInt(300));
  i += 321 == va_arg(ap, int);
  i += 44 == va_arg(ap, _BitInt(400));
  va_end(ap);
  return i;
}
