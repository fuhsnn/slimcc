#include "test.h"
#include <stdio.h>
#include <pthread.h>

static _Thread_local int tentative_tls;
static _Thread_local int tentative_tls = 3;

extern _Thread_local int extern_tls;

void *thread_main(void *unused) {

  static _Thread_local int v1;
  static _Thread_local int v2 = 5;

  ASSERT(0, v1);
  ASSERT(5, v2);

  ASSERT(3, tentative_tls);
  ASSERT(0, extern_tls);

  v1 = 1;
  v2 = 2;
  tentative_tls = 9;
  extern_tls = 7;

  ASSERT(1, v1);
  ASSERT(2, v2);

  ASSERT(9, tentative_tls);
  ASSERT(7, extern_tls);

  return NULL;
}

int main() {
  pthread_t thr1;
  pthread_t thr2;

  ASSERT(0, pthread_create(&thr1, NULL, thread_main, NULL));

  thread_main(NULL);

  ASSERT(0, pthread_create(&thr2, NULL, thread_main, NULL));

  ASSERT(0, pthread_join(thr1, NULL));
  ASSERT(0, pthread_join(thr2, NULL));

  printf("OK\n");
  return 0;
}
