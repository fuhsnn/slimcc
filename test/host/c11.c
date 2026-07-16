#include <stdarg.h>
#include <stdint.h>

_Thread_local int extern_tls;

typedef struct {
  _Alignas(128) char c;
} Ov;

int overaligned(Ov s1, long double d1, long double d2, Ov s2, Ov s3, long double d3, Ov s4, ...) {
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

typedef struct { } Empty1;
typedef struct { int i[0]; } Empty2;
typedef struct { int64_t a[16]; } Big;

int64_t empty_struct_arg(int x, Empty1 s1, int y, Empty2 s2, int z, Big b) {
  return x * 10000 + y * 1000 + z * 100 + b.a[0] * 10 + b.a[15];
}
