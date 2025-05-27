#include "test.h"

#define COND_PTR_TYPE(_val, _arg1, _arg2)  \
    SASSERT(_val == _Generic(0 ? (_arg1) : (_arg2), nullptr_t:1, void*:2, int*:3))

COND_PTR_TYPE(2, (void*)0, (void*)0);
COND_PTR_TYPE(1, nullptr, nullptr);
COND_PTR_TYPE(2, nullptr, (void*)0);
COND_PTR_TYPE(3, nullptr, (int*)0);

COND_PTR_TYPE(2, (void*)nullptr, (void*)0);
COND_PTR_TYPE(2, (void*)nullptr, 0);
COND_PTR_TYPE(2, (void*)nullptr, (int*)0);

COND_PTR_TYPE(3, (int*)nullptr, (int*)0);
COND_PTR_TYPE(3, (int*)nullptr, (void*)0);
COND_PTR_TYPE(3, (int*)nullptr, 0);
COND_PTR_TYPE(3, (int*)nullptr, nullptr);
COND_PTR_TYPE(2, (int*)nullptr, (void*)nullptr);

COND_PTR_TYPE(1, nullptr, (nullptr_t)(void*)0);
COND_PTR_TYPE(1, nullptr, (nullptr_t)0);
COND_PTR_TYPE(1, nullptr, (nullptr_t)nullptr);

nullptr_t p01;
nullptr_t p02 = 0;
nullptr_t p03 = {};
nullptr_t p04 = {0};

COND_PTR_TYPE(1, p02, nullptr);
COND_PTR_TYPE(2, p02, (void*)0);
COND_PTR_TYPE(3, p02, (int*)0);

constexpr nullptr_t p12 = 0;
constexpr nullptr_t p13 = {};
constexpr nullptr_t p14 = {0};

COND_PTR_TYPE(1, p12, nullptr);
COND_PTR_TYPE(2, p12, (void*)0);
COND_PTR_TYPE(3, p12, (int*)0);

SASSERT(p12 == nullptr);
SASSERT(p13 == 0);
SASSERT(p14 == NULL);

typedef nullptr_t n2;
n2 np2 = 0;
COND_PTR_TYPE(1, np2, nullptr);
COND_PTR_TYPE(2, np2, (void*)0);
COND_PTR_TYPE(3, np2, (int*)0);

static nullptr_t val_test(void *vp, nullptr_t np, n2 np2) {
  {
    int b1 = 0, b2 = 0, b3 = 0, b4 = 0;
    if (nullptr)
      b1 = 1;
    if (nullptr != vp)
      b2 = 1;
    if (!nullptr)
      b3 = 1;
    if ((bool)nullptr)
      b4 = 1;
    (void)nullptr;

    ASSERT(0, b1);
    ASSERT(0, b2);
    ASSERT(1, b3);
    ASSERT(0, b4);
  }

  {
    int b1 = 0, b2 = 0, b3 = 0, b4 = 0;
    if (np)
      b1 = 1;
    if (np != vp)
      b2 = 1;
    if (!np)
      b3 = 1;
    if ((bool)np)
      b4 = 1;
    (void)np;

    ASSERT(0, b1);
    ASSERT(0, b2);
    ASSERT(1, b3);
    ASSERT(0, b4);
  }

  {
    int b1 = 0, b2 = 0, b3 = 0, b4 = 0;
    if (np2)
      b1 = 1;
    if (np2 != vp)
      b2 = 1;
    if (!np2)
      b3 = 1;
    if ((bool)np2)
      b4 = 1;
    (void)np2;

    ASSERT(0, b1);
    ASSERT(0, b2);
    ASSERT(1, b3);
    ASSERT(0, b4);
  }

  return nullptr;
}

int main(void){
  nullptr_t p21;
  nullptr_t p22 = 0;
  nullptr_t p23 = {};
  nullptr_t p24 = {0};

  ASSERT(1, p22 == 0);
  ASSERT(1, p23 == NULL);
  ASSERT(1, p24 == nullptr);

  static nullptr_t p31;
  static nullptr_t p32 = 0;
  static nullptr_t p33 = {};
  static nullptr_t p34 = {0};

  ASSERT(1, p32 == 0);
  ASSERT(1, p33 == NULL);
  ASSERT(1, p34 == nullptr);

  constexpr nullptr_t p42 = 0;
  constexpr nullptr_t p43 = {};
  constexpr nullptr_t p44 = {0};

  DASSERT(p42 == 0);
  DASSERT(p43 == NULL);
  DASSERT(p44 == nullptr);

  ASSERT(1, !val_test(NULL, nullptr, 0));

  printf("OK\n");
}

