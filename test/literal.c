#include "test.h"

int main() {
  EASSERT(97, 'a');
  EASSERT(10, '\n');
  EASSERT(-128, '\x80');

  EASSERT(511, 0777);
  EASSERT(0, 0x0);
  EASSERT(10, 0xa);
  EASSERT(10, 0XA);
  EASSERT(48879, 0xbeef);
  EASSERT(48879, 0xBEEF);
  EASSERT(48879, 0XBEEF);
  EASSERT(0, 0b0);
  EASSERT(1, 0b1);
  EASSERT(47, 0b101111);
  EASSERT(47, 0B101111);

  EASSERT(4, sizeof(0));
  EASSERT(8, sizeof(0L));
  EASSERT(8, sizeof(0LU));
  EASSERT(8, sizeof(0UL));
  EASSERT(8, sizeof(0LL));
  EASSERT(8, sizeof(0LLU));
  EASSERT(8, sizeof(0Ull));
  EASSERT(8, sizeof(0l));
  EASSERT(8, sizeof(0ll));
  EASSERT(8, sizeof(0x0L));
  EASSERT(8, sizeof(0b0L));
  EASSERT(4, sizeof(2147483647));
  EASSERT(8, sizeof(2147483648));
  EASSERT(-1, 0xffffffffffffffff);
  EASSERT(8, sizeof(0xffffffffffffffff));
  EASSERT(4, sizeof(4294967295U));
  EASSERT(8, sizeof(4294967296U));

  EASSERT(3, -1U>>30);
  EASSERT(3, -1Ul>>62);
  EASSERT(3, -1ull>>62);

  EASSERT(1, 0xffffffffffffffffl>>63);
  EASSERT(1, 0xffffffffffffffffll>>63);

  EASSERT(-1, 18446744073709551615);
  EASSERT(8, sizeof(18446744073709551615));
  EASSERT(-1, 18446744073709551615>>63);

  EASSERT(-1, 0xffffffffffffffff);
  EASSERT(8, sizeof(0xffffffffffffffff));
  EASSERT(1, 0xffffffffffffffff>>63);

  EASSERT(-1, 01777777777777777777777);
  EASSERT(8, sizeof(01777777777777777777777));
  EASSERT(1, 01777777777777777777777>>63);

  EASSERT(-1, 0b1111111111111111111111111111111111111111111111111111111111111111);
  EASSERT(8, sizeof(0b1111111111111111111111111111111111111111111111111111111111111111));
  EASSERT(1, 0b1111111111111111111111111111111111111111111111111111111111111111>>63);

  EASSERT(8, sizeof(2147483648));
  EASSERT(4, sizeof(2147483647));

  EASSERT(8, sizeof(0x1ffffffff));
  EASSERT(4, sizeof(0xffffffff));
  EASSERT(1, 0xffffffff>>31);

  EASSERT(8, sizeof(040000000000));
  EASSERT(4, sizeof(037777777777));
  EASSERT(1, 037777777777>>31);

  EASSERT(8, sizeof(0b111111111111111111111111111111111));
  EASSERT(4, sizeof(0b11111111111111111111111111111111));
  EASSERT(1, 0b11111111111111111111111111111111>>31);

  EASSERT(-1, 1 << 31 >> 31);
  EASSERT(-1, 01 << 31 >> 31);
  EASSERT(-1, 0x1 << 31 >> 31);
  EASSERT(-1, 0b1 << 31 >> 31);

  0.0;
  1.0;
  3e+8;
  0x10.1p0;
  .1E4f;

  EASSERT(4, sizeof(8f));
  EASSERT(4, sizeof(0.3F));
  EASSERT(8, sizeof(0.));
  EASSERT(8, sizeof(.0));
  EASSERT(16, sizeof(5.l));
  EASSERT(16, sizeof(2.0L));

  test_assert(1, size\
of(char), \
         "sizeof(char)");

  EASSERT(4, sizeof(L'\0'));
  EASSERT(97, L'a');

  printf("OK\n");
  return 0;
}
