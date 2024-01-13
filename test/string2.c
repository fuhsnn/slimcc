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

  ASSERT(7 * sizeof(char), sizeof(str));
  ASSERT(7 * sizeof(char16_t), sizeof(str16));
  ASSERT(7 * sizeof(char32_t), sizeof(str32));
  ASSERT(7 * sizeof(wchar_t), sizeof(strw));

  char *arr[] = { "fo", {"oba"}, ("rfoob"), {(("ar"))} };
  ASSERT(1, !strcmp("fo", arr[0]) );
  ASSERT(1, !strcmp("oba", arr[1]) );
  ASSERT(1, !strcmp("rfoob", arr[2]) );
  ASSERT(1, !strcmp("ar", arr[3]) );

  struct S {
    char *p;
    char a[4];
    int i;
  };
  struct S sarr[] = {
    "foobar", "baz", 11,
    ("xxx","foobaz"), (("bar")), 22,
  };
  ASSERT(1, !strcmp("foobar", sarr[0].p) );
  ASSERT(1, !strcmp("baz", sarr[0].a) );
  ASSERT(1, !strcmp("foobaz", sarr[1].p) );
  ASSERT(1, !strcmp("bar", sarr[1].a) );

  printf("OK\n");
}
