#include "test.h"
#include "stddef.h"

[[_Noreturn]]
int fallthrough(int i) {
  switch (i){
    case 3:
    [[fallthrough]];
    default:
  }
  exit(0);
}

void packed(void) {
  ASSERT(0, offsetof(struct [[gnu::packed]] { char a; int b; }, a));
  ASSERT(1, offsetof(struct [[gnu::packed]] { char a; int b; }, b));
  ASSERT(5, ({ struct [[gnu::packed]] { char a; int b; } x; sizeof(x); }));
  ASSERT(9, ({ typedef struct [[gnu::packed]] { char a; int b[2]; } T; sizeof(T); }));
  ASSERT(1, offsetof(struct [[gnu::packed]] T { char a; int b[2]; }, b));

  // no-op
  ASSERT(8, ({ struct { char a; int b; } [[gnu::packed]] x; sizeof(x); }));
  ASSERT(0, offsetof(struct { char a; int b; } [[gnu::packed]], a));
  ASSERT(4, offsetof(struct { char a; int b; } [[gnu::packed]], b));
  ASSERT(12, ({ typedef struct [[packed]] { char a; int b[2]; } T; sizeof(T); }));
  ASSERT(4, offsetof(struct [[packed]] { char a; int b[2]; }, b));
}

int main() {
  packed();

  printf("OK\n");
  return 0;
}
