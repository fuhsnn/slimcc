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

  ASSERT('f', str[0]);
  ASSERT('o', str[1]);
  ASSERT('o', str16[2]);
  ASSERT('b', str32[3]);
  ASSERT('a', strw[4]);

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

  {
    struct Flex { int i;  char arr[]; };
    static struct Flex gflex = {12, "345"};

    ASSERT(12, gflex.i);
    ASSERT('3', gflex.arr[0]);
    ASSERT('4', gflex.arr[1]);
    ASSERT('5', gflex.arr[2]);
    ASSERT('\0', gflex.arr[3]);

#if defined(__slimcc__) || defined(__widcc__)
    struct Flex lflex = {6, "789"};
    ASSERT(6, lflex.i);
    ASSERT('7', lflex.arr[0]);
    ASSERT('8', lflex.arr[1]);
    ASSERT('9', lflex.arr[2]);
    ASSERT('\0', lflex.arr[3]);
#endif
  }

  {
    char c1 = "foobar"[4];
    char c2 = { "foobar"[4] };
    char a1[] = { "foobar"[4] };
    static char sc1 = "foobar"[4];
    static char sc2 = { "foobar"[4] };
    static char sa1[] = { "foobar"[4] };
    ASSERT('a', c1);
    ASSERT('a', c2);
    ASSERT('a', sc1);
    ASSERT('a', sc1);
    ASSERT('a', a1[0]);
    ASSERT('a', sa1[0]);
    ASSERT(1, sizeof(a1));
    ASSERT(1, sizeof(sa1));
  }

  printf("OK\n");
}
