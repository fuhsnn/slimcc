#include "test.h"

#ifndef __has_builtin
#error
#endif

#if !__has_builtin(__builtin_offsetof)
#error
#endif

struct ofs_S1 {
  char c;
  struct {
  struct {
    struct {
    struct {
      char k[7];
    } n;
    };
  } m[4][4];
  };
};
_Static_assert(__builtin_offsetof(struct ofs_S1, m[1][2].n.k[3]) == 46,"");


int va(int i, ...) {
  __builtin_va_list ap;
  __builtin_va_start(ap, (i += 7, i));
#ifdef __slimcc__
  __builtin_va_start(ap);
#endif
  __builtin_va_end((i += 13, ap));
  return i;
}

int main(void) {
  ASSERT(30, va(17));

  printf("OK\n");
}
