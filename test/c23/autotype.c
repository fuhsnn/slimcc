#include "test.h"

static auto d = 3.5;
__auto_type dp = &d;
static_assert(_Generic(d, double:1));
static_assert(_Generic(dp, double *:1));

char arr[17];
auto p1 = arr;
auto p2 = &arr;
static_assert(8 == sizeof(p1));
static_assert(1 == sizeof(*p1));
static_assert(8 == sizeof(p2));
static_assert(17 == sizeof(*p2));

int main(void) {
  typedef struct S S;
  auto sp = (S*)NULL;
  struct S { int i[17]; };
  S s = {.i[13] = 19};
  sp = (void*)&s;
  ASSERT(19, sp->i[13]);

  typedef char foo;
  {
    __auto_type foo = 0L;
    static_assert(_Generic(foo, long:1));

    for (auto foo = 17.9;;) {
      static_assert(_Generic(foo, double:1));
      break;
    }
  }

  printf("OK\n");
}


