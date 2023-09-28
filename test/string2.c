#include "test.h"

#include <uchar.h>
#include <wchar.h>

char str[] = {"foobar"};
char16_t str16[] = (u"foobar");
char32_t str32[] = ((U"foobar"));
wchar_t strw[] = {((L"foobar"))};

int main(void){
  ASSERT(1, _Generic( str, char *: 1));
  ASSERT(1, _Generic( str16, char16_t *: 1));
  ASSERT(1, _Generic( str32, char32_t *: 1));
  ASSERT(1, _Generic( strw, wchar_t *: 1));

  ASSERT(sizeof(str), 7 * sizeof(char) );
  ASSERT(sizeof(str16), 7 * sizeof(char16_t) );
  ASSERT(sizeof(str32), 7 * sizeof(char32_t) );
  ASSERT(sizeof(strw), 7 * sizeof(wchar_t) );

  char *arr[] = { "fo", {"oba"}, ("rfoob"), {(("ar"))} };
  ASSERT(0, strncmp("fo\0", arr[0], 3) );
  ASSERT(0, strncmp("oba\0", arr[1], 4) );
  ASSERT(0, strncmp("rfoob\0", arr[2], 6) );
  ASSERT(0, strncmp("ar\0", arr[3], 3) );

  printf("OK\n");
}
