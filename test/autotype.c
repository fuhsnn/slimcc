#include "test.h"

static auto d = 3.5;
__auto_type dp = &d;
static_assert(_Generic(d, double:1));
static_assert(_Generic(dp, double *:1));

char arr[17] = {[3] = 33, [11] = 1};
auto p1 = arr;
auto p2 = &arr;
static_assert(8 == sizeof(p1));
static_assert(1 == sizeof(*p1));
static_assert(8 == sizeof(p2));
static_assert(17 == sizeof(*p2));

int main(void) {

  ASSERT(33, p1[3]);
  ASSERT(0, p1[7]);
  ASSERT(1, p1[11]);
  ASSERT(33, (*p2)[3]);
  ASSERT(0, (*p2)[7]);
  ASSERT(1, (*p2)[11]);

  typedef struct S S;
  auto sp = (S*)NULL;
  struct S { int i[17]; };
  S s = {.i[13] = 19};
  sp = (void*)&s;
  ASSERT(19, sp->i[13]);

  typedef char foo;
  auto foo f0;
  static_assert(_Generic(f0, char:1));
  {
    __auto_type foo = 0L;
    static_assert(_Generic(foo, long:1));

    for (auto foo = 17.9;;) {
      static_assert(_Generic(foo, double:1));
      break;
    }
  }

  ASSERT(1, ({const int init = 0; auto i = init; _Generic(&i, int *:1);}));
  ASSERT(1, ({const int init = 0; const auto i = init; _Generic(&i, int const*:1);}));

  {
    constexpr auto v1 = 123LL;
    SASSERT(_Alignof(v1) == _Alignof(long long));
  }
  {
    struct { int i; float f; } s1 = {1, 2.5};
    __auto_type s2 = s1;
    ASSERT(1, s2.i);
    ASSERT(1, s2.f == 2.5f);

    __auto_type sp = &s1;
    ASSERT(1, sp->i);
    ASSERT(1, sp->f == 2.5f);

    __auto_type s3 = *sp;
    ASSERT(1, s3.i);
    ASSERT(1, s3.f == 2.5f);
  }

  printf("OK\n");
}


