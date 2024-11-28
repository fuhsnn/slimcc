#include "test.h"

enum e0 {
    e0_i32max = 0x7FFFFFFF,
};
_Static_assert(_Generic(e0_i32max, int32_t:1), "");
_Static_assert(_Generic((enum e0)0, uint32_t:1), "");

enum e1 {
    e1_i32max = 0x7FFFFFFF,
    e1_i32max_plus1,
};
_Static_assert(_Generic(e1_i32max, uint32_t:1), "");
_Static_assert(_Generic((enum e1)0, uint32_t:1), "");

enum e2 {
    e2_neg = -1,
    e2_i32max = 0x7FFFFFFF,
    e2_i32max_plus1,
};
_Static_assert(_Generic(e2_i32max, int64_t:1), "");
_Static_assert(_Generic((enum e2)0, int64_t:1), "");

enum e3 {
    e3_i32max = 0x7FFFFFFF,
    e3_neg = -1
};
_Static_assert(_Generic(e3_i32max, int32_t:1), "");
_Static_assert(_Generic((enum e3)0, int32_t:1), "");

enum e4 {
    e4_u32max = 0xFFFFFFFF,
};
_Static_assert(_Generic(e4_u32max, uint32_t:1), "");
_Static_assert(_Generic((enum e4)0, uint32_t:1), "");

enum e5 {
    e5_u32max = 0xFFFFFFFF,
    e5_neg = -1
};
_Static_assert(_Generic(e5_u32max, int64_t:1), "");
_Static_assert(_Generic((enum e5)0, int64_t:1), "");

enum e6 {
    e6_u32max = 0xFFFFFFFF,
    e6_u32max_plus1,
};
_Static_assert(_Generic(e6_u32max_plus1, uint64_t:1), "");
_Static_assert(_Generic((enum e6)0, uint64_t:1), "");

enum e7 {
    e7_u32max = 0xFFFFFFFF,
    e7_u32max_plus1,
    e7_neg = -1
};
_Static_assert(_Generic(e7_u32max_plus1, int64_t:1), "");
_Static_assert(_Generic((enum e7)0, int64_t:1), "");

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

  printf("OK\n");
}
