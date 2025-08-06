#include <stdarg.h>

_Thread_local int extern_tls;

typedef struct {
  _Alignas(128) char c;
} Ov;

int overaligned_0(Ov s1, long double d1, long double d2, Ov s2, Ov s3, long double d3, Ov s4, ...);
int overaligned_0(Ov s1, long double d1, long double d2, Ov s2, Ov s3, long double d3, Ov s4, ...) {
  va_list ap;
  va_start(ap, s4);
  long double ret = s1.c + d1 + d2 + s2.c + s3.c + d3 + s4.c;
  ret += va_arg(ap, long double);
  ret += va_arg(ap, Ov).c;
  ret += va_arg(ap, long double);
  ret += va_arg(ap, long double);
  ret += va_arg(ap, Ov).c;
  ret += va_arg(ap, Ov).c;
  va_end(ap);
  return ret;
}
