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


int va_expr_in_arg(int i, ...) {
  __builtin_va_list ap, ap2;
  __builtin_va_start(ap, (i += 7, i));
  __builtin_va_copy(ap2, ap);
  ASSERT(1, !memcmp(ap,ap2,sizeof(__builtin_va_list)));
#ifdef __slimcc__
  __builtin_va_start(ap);
#endif
  __builtin_va_end((i += 13, ap));
  return i;
}

int va_in_comma(int i, ...) {
  __builtin_va_list ap, ap2;
  return __builtin_va_start(ap, i),
  __builtin_va_copy(ap2, ap),
  i = __builtin_va_arg(ap2, int),
  __builtin_va_end(ap2),
  i;
}

int main(void) {
  ASSERT(30, va_expr_in_arg(17));
  ASSERT(33, va_in_comma(0,33));

  printf("OK\n");
}
