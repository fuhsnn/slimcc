#include "test.h"

#if (__SIZEOF_INT__ != 4)
#error
#endif

#if (__SIZEOF_LONG__ > 4)
#define FIRST_64BIT_INT long
#else
#define FIRST_64BIT_INT long long
#endif

int extend(void) {
  enum e0 {
    e0_i32max = 0x7FFFFFFF,
  };
  SASSERT(_Generic(e0_i32max, int32_t:1));
  SASSERT(_Generic((enum e0)0, uint32_t:1));

  enum e1 {
    e1_i32max = 0x7FFFFFFF,
    e1_i32max_plus1,
  };
  SASSERT(_Generic(e1_i32max, uint32_t:1));
  SASSERT(_Generic((enum e1)0, uint32_t:1));

  enum e2 {
    e2_neg = -1,
    e2_i32max = 0x7FFFFFFF,
    e2_i32max_plus1,
  };
  SASSERT(_Generic(e2_i32max, FIRST_64BIT_INT:1));
  SASSERT(_Generic((enum e2)0, FIRST_64BIT_INT:1));

  enum e3 {
    e3_i32max = 0x7FFFFFFF,
    e3_neg = -1
  };
  SASSERT(_Generic(e3_i32max, int32_t:1));
  SASSERT(_Generic((enum e3)0, int32_t:1));

  enum e4 {
    e4_u32max = 0xFFFFFFFF,
  };
  SASSERT(_Generic(e4_u32max, uint32_t:1));
  SASSERT(_Generic((enum e4)0, uint32_t:1));

  enum e5 {
    e5_u32max = 0xFFFFFFFF,
    e5_neg = -1
  };
  SASSERT(_Generic(e5_u32max, FIRST_64BIT_INT:1));
  SASSERT(_Generic((enum e5)0, FIRST_64BIT_INT:1));

  enum e6 {
    e6_u32max = 0xFFFFFFFF,
    e6_u32max_plus1,
  };
  SASSERT(_Generic(e6_u32max_plus1, unsigned FIRST_64BIT_INT:1));
  SASSERT(_Generic((enum e6)0, unsigned FIRST_64BIT_INT:1));

  enum e7 {
    e7_u32max = 0xFFFFFFFF,
    e7_u32max_plus1,
    e7_neg = -1
  };
  SASSERT(_Generic(e7_u32max_plus1, FIRST_64BIT_INT:1));
  SASSERT(_Generic((enum e7)0, FIRST_64BIT_INT:1));

  return 1;
}

static int compat(void) {
  {
    enum { A };
    enum { B };
    SASSERT(1 == _Generic(       A , typeof(B):1, default:0));
    SASSERT(1 == _Generic(typeof(A), typeof(B):1, default:0));
  }
  {
    SASSERT(0 == _Generic(typeof(enum { A }), typeof(enum { B }):1, default:0));
  }
  {
    enum : int { A };
    enum : int { B };
    SASSERT(0 == _Generic(       A , typeof(B):1, default:0));
    SASSERT(0 == _Generic(typeof(A), typeof(B):1, default:0));
  }
  {
    SASSERT(0 == _Generic(typeof(enum : int { A }), typeof(enum : int { B }):1, default:0));
  }
  {
    enum { A = (long long)-1 };
    enum { B = (long long)-1 };
    SASSERT(1 == _Generic(       A , typeof(B):1, default:0));
    SASSERT(1 == _Generic(typeof(A), typeof(B):1, default:0));
  }
  {
    enum { A = INT64_MIN };
    enum { B = INT64_MIN };
    SASSERT(0 == _Generic(       A , typeof(B):1, default:0));
    SASSERT(0 == _Generic(typeof(A), typeof(B):1, default:0));
  }
  {
    typedef enum E { A } e1;
    {
      enum E e;

      SASSERT(1 == _Generic(       e,  e1:1, default:0));
      SASSERT(1 == _Generic(typeof(e), e1:1, default:0));
      SASSERT(1 == _Generic(enum E,    e1:1, default:0));
    }
    {
      enum E { A } e;

      SASSERT(0 == _Generic(       A,  e1:1, default:0));
      SASSERT(0 == _Generic(typeof(A), e1:1, default:0));
      SASSERT(1 == _Generic(       e,  e1:1, default:0));
      SASSERT(1 == _Generic(typeof(e), e1:1, default:0));
      SASSERT(1 == _Generic(enum E,    e1:1, default:0));
    }
    {
      enum E:int;

      SASSERT(0 == _Generic(enum E,    e1:1, default:0));
    }
    {
      enum E:int { A } e;

      SASSERT(0 == _Generic(       A,  e1:1, default:0));
      SASSERT(0 == _Generic(typeof(A), e1:1, default:0));
      SASSERT(0 == _Generic(       e,  e1:1, default:0));
      SASSERT(0 == _Generic(typeof(e), e1:1, default:0));
      SASSERT(0 == _Generic(enum E,    e1:1, default:0));
    }
  }
  {
    typedef enum E:int { A } e1;
    {
      enum E e;

      SASSERT(1 == _Generic(       e,  e1:1, default:0));
      SASSERT(1 == _Generic(typeof(e), e1:1, default:0));
      SASSERT(1 == _Generic(enum E,    e1:1, default:0));
    }
    {
      enum E { A } e;

      SASSERT(1 == _Generic(       A,  e1:1, default:0));
      SASSERT(1 == _Generic(typeof(A), e1:1, default:0));
      SASSERT(0 == _Generic(       e,  e1:1, default:0));
      SASSERT(0 == _Generic(typeof(e), e1:1, default:0));
      SASSERT(0 == _Generic(enum E,    e1:1, default:0));
    }
    {
      enum E:int;

      SASSERT(0 == _Generic(enum E,    e1:1, default:0));
    }
    {
      enum E:int { A } e;

      SASSERT(1 == _Generic(       A,  e1:1, default:0));
      SASSERT(1 == _Generic(typeof(A), e1:1, default:0));
      SASSERT(1 == _Generic(       e,  e1:1, default:0));
      SASSERT(1 == _Generic(typeof(e), e1:1, default:0));
      SASSERT(1 == _Generic(enum E,    e1:1, default:0));
    }
  }
  return 1;
}

static int redecl(void) {
  {
    enum E;
    typedef enum E {A, B} e;
    {
      enum E;
      SASSERT(1 == _Generic(enum E, e:1, default:0));
      enum E {A, B};
      SASSERT(1 == _Generic(enum E, e:1, default:0));
    }
    {
      enum E;
      enum E {A};
      SASSERT(0 == _Generic(enum E, e:1, default:0));
    }
  }
  {
    enum E : int;
    typedef enum E : int {A, B} e;
    {
      enum E : int;
      SASSERT(0 == _Generic(enum E, e:1, default:0));
      enum E : int {A, B};
      SASSERT(1 == _Generic(enum E, e:1, default:0));
    }
    {
      enum E : int;
      enum E : int {A};
      SASSERT(0 == _Generic(enum E, e:1, default:0));
    }
  }
  {
    typedef enum E e1;
    enum E { A } e;
    typedef enum E e2;
    typedef enum E { A } e3;
    SASSERT(_Generic(e, e1:1));
    SASSERT(_Generic(e, e2:1));
    SASSERT(_Generic(e, e3:1));
  }
  {
    typedef enum E : long { A } e1;
    typedef const enum E e2;
    e2 e;
    typedef const enum E : long { A } e3;
    SASSERT(_Generic(e, e1:1));
    SASSERT(_Generic(e, typeof_unqual(e2):1));
    SASSERT(_Generic(typeof_unqual(e3), e1:1));
    SASSERT(_Generic(e2, e3:1));
    SASSERT(_Generic(const e1, typeof(e):1));
    SASSERT(_Generic(&e, enum E : long { A } const *:1));
  }
  return 1;
}

int main(void) {
  enum E1;
  typedef enum E1 const C2;
  typedef C2 *P;
  P p;

  enum E1 {
    E1A,
    E1B = 5000000000,
  };

  DASSERT(sizeof(*p) == 8);
  DASSERT(5000000000 == E1B);

  enum E2 : short;
  enum E2 : short { E2A };
  DASSERT(sizeof(E2A) == 2);

  enum : short unsigned { ANON_E };
  DASSERT(sizeof(ANON_E) == 2);

  ASSERT(1, extend());
  ASSERT(1, compat());
  ASSERT(1, redecl());

  printf("OK\n");
}
