#include "test.h"
#include <pthread.h>

extern _Thread_local int extern_tls;

static _Thread_local int tentative_tls;
static _Thread_local int tentative_tls = 3;

void *thread_main(void *unused) {
  ASSERT(0, extern_tls);
  ASSERT(3, tentative_tls);

  extern_tls = 7;
  tentative_tls = 9;

  ASSERT(7, extern_tls);
  ASSERT(9, tentative_tls);

  return NULL;
}

int tls(void) {
  pthread_t thr1;
  pthread_t thr2;

  ASSERT(0, pthread_create(&thr1, NULL, thread_main, NULL));

  thread_main(NULL);

  ASSERT(0, pthread_create(&thr2, NULL, thread_main, NULL));

  ASSERT(0, pthread_join(thr1, NULL));
  ASSERT(0, pthread_join(thr2, NULL));

  return 1;
}

typedef struct {
  _Alignas(128) char c;
} Ov;

int overaligned_0(Ov s1, long double d1, long double d2, Ov s2, Ov s3, long double d3, Ov s4, ...);
int overaligned_1(Ov s1, long double d1, long double d2, Ov s2, Ov s3, long double d3, Ov s4, ...) {
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

int main(void) {
  ASSERT(1, tls());

  { Ov s = {.c = 33}; ASSERT(316, overaligned_0(s, 1, 2, s, s, 3, s, (long double)55, s, (long double)66, (long double)-42, s, s)); };
  { Ov s = {.c = 33}; ASSERT(316, overaligned_1(s, 1, 2, s, s, 3, s, (long double)55, s, (long double)66, (long double)-42, s, s)); };

  printf("OK\n");
}
