#include "test.h"
#include <stddef.h>
struct S {
  int : 3;
  struct Inner{
        int :3;
  } i;
  _Static_assert(sizeof(struct Inner) == 1);
};

_Static_assert(offsetof(struct S, i) == 1);

static_assert(sizeof (char[]){1,2,3} == 3);

int main(void) {
  for (_Static_assert(1); 0;) {
    _Static_assert(1, "msg");
  }

  for (static_assert(1), static_assert(1);
    static_assert(1), 0;
    static_assert(1), static_assert(1))
   static_assert(1);

  static_assert(1), static_assert(1);
  (static_assert(1)), static_assert(1);
  static_assert(1), (static_assert(1));
  (static_assert(1, "msg"), static_assert(1));
  static_assert(1),  ({ static_assert(1, "msg"); });
  ({ static_assert(1), static_assert(1, "msg"); });

  static_assert(2==_Generic(static_assert(1==_Generic(static_assert(1), void:1)), void:2));

  printf("OK\n");
  return 0;
}
