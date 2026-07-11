#include "test.h"
#include <stdatomic.h>

int ptr_arith(void) {
    int32_t *_Atomic p = NULL;
    int32_t *p2 = atomic_fetch_add(&p, 1);
#ifdef NOTGCC
    ASSERT(4, (long)p);
#endif
    ASSERT(0, (long)p2);
  return 1;
}

int f32(void) {
  _Atomic float f = 5.0f;

  ASSERT(1, 8.0f == (f += 3.0f));
#ifdef NOTGCC
  ASSERT(1, 8.0f == atomic_fetch_sub(&f, 2.0f));
  ASSERT(1, 6.0f == f);
#endif
  ASSERT(1, 7.0f == (f = 7.0f));
  ASSERT(1, 7.0f == f);
  return 1;
}

int f64(void) {
  _Atomic double d = 10.0;

  ASSERT(1, 7.0 == (d -= 3.0));
#ifdef NOTGCC
  ASSERT(1, 7.0 == atomic_fetch_add(&d, 2.0));
  ASSERT(1, 9.0 == d);
#endif
  ASSERT(1, 5.0 == (d = 5.0));
  ASSERT(1, 5.0 == d);
  return 1;
}

int array_loadstore(int cnt) {
  struct { _Atomic int i[1]; } s = { 3 };
  ASSERT(3, atomic_load(s.i));

  _Atomic int vla[cnt] = {};
  atomic_store(vla, 7);

  ASSERT(7, atomic_load(vla));
  ASSERT(0, atomic_load(vla + 1));
  return 1;
}

int implicit_cast() {
  _Atomic int p = 2;
  ASSERT(2, p);

  auto b = (p = 7.0);
  ASSERT(7, b);
  ASSERT(7, p);

  int a = (p += -9.0f);
  ASSERT(-2, a);
  ASSERT(-2, p);

  int c = atomic_exchange(&p, 7.0);
  ASSERT(7, p);
  ASSERT(-2, c);

  ASSERT(1, atomic_compare_exchange_weak(&p, &b, 3.0f));
  ASSERT(3, p);
  ASSERT(7, b);

  return 1;
}

int main(void) {
  {
    _Atomic enum : int {A} i;
    enum E : int;
    static_assert(_Generic(typeof(atomic_exchange(&i, 0)), enum E: 0, default: 1));
    static_assert(_Generic(typeof(atomic_exchange(&i, 0)), int: 1, default: 0));
  }

  ASSERT(1, ptr_arith());
  ASSERT(1, f32());
  ASSERT(1, f64());
  ASSERT(1, array_loadstore(2));
  ASSERT(1, implicit_cast());

  void *p;
  //SREJ atomic_load(p);
  //SREJ atomic_store(p, 0);
  //SREJ atomic_fetch_add(p, 0);
  //SREJ atomic_exchange(p, 0);
  //SREJ atomic_compare_exchange_weak(p, &(int){}, 0);

  printf("OK\n");
}
