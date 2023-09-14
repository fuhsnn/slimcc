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

int main(void) {
  for (_Static_assert(1); 0;) {
    _Static_assert(1, "msg");
  }
  printf("OK\n");
  return 0;
}
