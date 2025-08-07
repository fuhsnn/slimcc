#include "test.h"

#include <uchar.h>
#include <wchar.h>

#define STR(x) #x

int main(void) {
  ASSERT(u8'a', 'a');
  SASSERT(_Generic(u8'b', unsigned char:1));

  #define CAT(x,y) x##y
  int CAT(b, 6_) = 7;
  int CAT(b, 8µ) = 9;
  ASSERT(7, b6_);
  ASSERT(9, b8µ);

  ASSERT(181, u'\u00b5');
  ASSERT(129437, U'\U0001F99D');

  ASSERT(0x7777, 'w\u0077');
  ASSERT(0x777777, '\u0077\U00000077w');
  ASSERT(0x77777777, '\x77\u0077w\U00000077');
  { long v = '\x80\x80\x80\x80'; ASSERT(1, v < 0); }

  static const char str1[] = STR('\U0001F99D');
  static const char ref1[] = {0x27, 0x5c, 0x55, 0x30, 0x30, 0x30, 0x31, 0x46, 0x39, 0x39, 0x44, 0x27, 0};

  static const char str2[] = STR("\u00b5\u28e1\U0001f0a1");
  static const char ref2[] = {0x22, 0x5c, 0x75, 0x30, 0x30, 0x62, 0x35, 0x5c, 0x75, 0x32, 0x38,
    0x65, 0x31, 0x5c, 0x55, 0x30, 0x30, 0x30, 0x31, 0x66, 0x30, 0x61, 0x31, 0x22, 0};

  static const char str3[] = STR(\U0001F99D);
  static const char ref3[] = {0xf0, 0x9f, 0xa6, 0x9d, 0};

  ASSERT(1, sizeof(str1) == sizeof(ref1) && !memcmp(&str1, &ref1, sizeof(ref1)));
  ASSERT(1, sizeof(str2) == sizeof(ref2) && !memcmp(&str2, &ref2, sizeof(ref2)));
  ASSERT(1, sizeof(str3) == sizeof(ref3) && !memcmp(&str3, &ref3, sizeof(ref3)));

  static const char tst8[] = "\u00b5\u28e1\U0001f0a1";
  static const char chk8[] = {0xc2, 0xb5, 0xe2, 0xa3, 0xa1, 0xf0, 0x9f, 0x82, 0xa1, 0};

  static const char16_t tst16[] = u"\u00b5\u28e1\U0001f0a1";
  static const char16_t chk16[] = {0xb5, 0x28e1, 0xd83c, 0xdca1, 0};

  static const char32_t tst32[] = U"\u00b5\u28e1\U0001f0a1";
  static const char32_t chk32[] = {0xb5, 0x28e1, 0x1f0a1, 0};

  ASSERT(1, sizeof(tst8) == sizeof(chk8) && !memcmp(&tst8, &chk8, sizeof(chk8)));
  ASSERT(1, sizeof(tst16) == sizeof(chk16) && !memcmp(&tst16, &chk16, sizeof(chk16)));
  ASSERT(1, sizeof(tst32) == sizeof(chk32) && !memcmp(&tst32, &chk32, sizeof(chk32)));

  int µ42µ = 33;
  \U000000b542\u00b5 += 6;
  ASSERT(39, \u00b542\U000000b5);

  int CAT(\U000000b5, \u00b5) = 44;

#ifdef NOTCLANG
  ASSERT(44, µµ);
#endif

  printf("OK\n");
}
