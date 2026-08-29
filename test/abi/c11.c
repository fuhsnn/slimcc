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

int overaligned(Ov s1, long double d1, long double d2, Ov s2, Ov s3, long double d3, Ov s4, ...);

typedef struct { } Empty1;
typedef struct { int i[0]; } Empty2;
typedef struct { int64_t a[16]; } Big;

int64_t empty_struct_arg(int x, Empty1 s1, int y, Empty2 s2, int z, Big b);

static void static_arr_size_oldstyle(n, a)
  int n, a[static n];
{}

int main(void) {
  ASSERT(1, tls());

  { Ov s = {.c = 33}; ASSERT(316, overaligned(s, 1, 2, s, s, 3, s, (long double)55, s, (long double)66, (long double)-42, s, s)); };

  {
    Empty1 e1;
    Empty2 e2;
    Big big = {.a = {[0] = 4, [15] = 5}};
    int i = 0;
    ASSERT(12345, empty_struct_arg(1,(i++,e1),2,(i+=20,e2),3,big));
    ASSERT(21, i);
  }

  printf("OK\n");
}
