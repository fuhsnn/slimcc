#include "test.h"

#include <stddef.h>
#include <uchar.h>
#include <wchar.h>

SASSERT(1 == _Generic(sizeof(int), size_t: 1));

static char *a;
SASSERT(1 == _Generic(a - a, ptrdiff_t: 1));

#if __STDC_VERSION__ >= 202311L
SASSERT(1 == _Generic(u8"s"[0], unsigned char: 1));
SASSERT(1 == _Generic(u8's',    unsigned char: 1));
#else
SASSERT(1 == _Generic(u8"s"[0], char: 1));
#endif

SASSERT(1 == _Generic(u"s"[0],  char16_t: 1));
SASSERT(1 == _Generic(u's',     char16_t: 1));

SASSERT(1 == _Generic(U"s"[0],  char32_t: 1));
SASSERT(1 == _Generic(U's',     char32_t: 1));

SASSERT(1 == _Generic(L"s"[0],  wchar_t: 1));
SASSERT(1 == _Generic(L's',     wchar_t: 1));

int main(void) {
  puts("OK");
}
