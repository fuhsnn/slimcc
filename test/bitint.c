#include "test.h"

SASSERT(sizeof(_BitInt(7)) == 1);
SASSERT(sizeof(_BitInt(15)) == 2);
SASSERT(sizeof(_BitInt(31)) == 4);
SASSERT(sizeof(_BitInt(63)) == 8);

SASSERT(_Alignof(_BitInt(7)) == 1);
SASSERT(_Alignof(_BitInt(15)) == 2);
SASSERT(_Alignof(_BitInt(31)) == 4);
SASSERT(_Alignof(_BitInt(63)) == 8);

SASSERT(sizeof(_BitInt(7)[17]) == 17);
SASSERT(sizeof(_BitInt(15)[17]) == 34);
SASSERT(sizeof(_BitInt(31)[17]) == 68);
SASSERT(sizeof(_BitInt(63)[17]) == 136);

#define CHK_USUAL_CONV(A, B, C) \
  SASSERT(_Generic((A)0+(B)0, C:1))

CHK_USUAL_CONV(_BitInt(2) signed, _BitInt(1) unsigned, _BitInt(2));
CHK_USUAL_CONV(_BitInt(2), unsigned _BitInt(2), unsigned _BitInt(2));

CHK_USUAL_CONV(_BitInt(2), _Bool, int);
CHK_USUAL_CONV(unsigned _BitInt(1), _Bool, int);

#define W (__SIZEOF_INT__ * 8)

CHK_USUAL_CONV(signed _BitInt(W-1), signed, signed);
CHK_USUAL_CONV(signed _BitInt(W-1), unsigned, unsigned);
CHK_USUAL_CONV(unsigned _BitInt(W-1), signed, signed);
CHK_USUAL_CONV(unsigned _BitInt(W-1), unsigned, unsigned);

CHK_USUAL_CONV(signed _BitInt(W), signed, signed);
CHK_USUAL_CONV(signed _BitInt(W), unsigned, unsigned);
CHK_USUAL_CONV(unsigned _BitInt(W), signed, unsigned);
CHK_USUAL_CONV(unsigned _BitInt(W), unsigned, unsigned);

CHK_USUAL_CONV(signed _BitInt(W+1), signed, signed _BitInt(W+1));
CHK_USUAL_CONV(signed _BitInt(W+1), unsigned, signed _BitInt(W+1));
CHK_USUAL_CONV(unsigned _BitInt(W+1), signed, unsigned _BitInt(W+1));
CHK_USUAL_CONV(unsigned _BitInt(W+1), unsigned, unsigned _BitInt(W+1));

void bitint_bitfiled(void) {
  ASSERT(1, ({struct { _BitInt(W-1)i:W-1;} s; _Generic(-s.i, _BitInt(W-1):1);}));
  ASSERT(1, ({struct { _BitInt( W )i:W-1;} s; _Generic(-s.i, _BitInt( W ):1);}));
  ASSERT(1, ({struct { _BitInt(W+1)i:W-1;} s; _Generic(-s.i, _BitInt(W+1):1);}));

  ASSERT(1, ({struct { _BitInt(W-1)i:W-1;} s; _Generic(0+s.i, int:1);}));
  ASSERT(1, ({struct { _BitInt( W )i:W-1;} s; _Generic(0+s.i, int:1);}));
  ASSERT(1, ({struct { _BitInt(W+1)i:W-1;} s; _Generic(0+s.i, _BitInt(W+1):1);}));
}

SASSERT(_Generic(0wb, _BitInt(2):1));
SASSERT(_Generic(0uwb, _BitInt(1) unsigned:1));
SASSERT(_Generic(0b11wb, _BitInt(3):1));
SASSERT(_Generic(0b10wbu, _BitInt(2) unsigned:1));
SASSERT(_Generic(012WBu, _BitInt(4) unsigned:1));
SASSERT(_Generic(0x0B2fUwb, _BitInt(12) unsigned:1));
SASSERT(_Generic(0xFFFF'FFFF'FFFF'FFFFwb, _BitInt(65):1));
SASSERT(_Generic(8989898989898989898989898989898989898WB, _BitInt(124):1));
SASSERT(_Generic(077777777777776666666666666666666667777777777uwb, _BitInt(132)unsigned:1));
SASSERT(_Generic(0x7fffffffffffffffEEEEEEEEABCabcfeeeeeeeFFFFFFFFFF555555wb, _BitInt(216):1));

typedef struct {
  _BitInt(256) a : 3, b : 6, c : 7;
} B;

constexpr B b_c = {-1, 2, -3};
B b_s = {1, -2, 3};

void bitint_bitfiled_var(int i1, int i2, int i3) {
  B b_r = {i1, i2, i3};
  ASSERT(1, b_c.a == b_r.c);
  ASSERT(1, b_c.b == b_r.b);
  ASSERT(1, b_c.c == b_r.a);

  ASSERT(1, b_s.a > b_r.c);
  ASSERT(1, b_s.b != b_r.b);
  ASSERT(0, b_s.c < b_r.a);

  ASSERT(-2, b_r.a *= b_s.b);
  ASSERT(1, b_s.a++);
  ASSERT(4, b_r.b *= b_s.a);
  ASSERT(-2, --b_r.c);
  ASSERT(-3, b_s.b += ({ b_c.c % b_r.c; }));
  ASSERT(-52, ({ b_r.b * b_s.b; }) - ( ({ b_s.c -= b_r.a; }) <<
    ({ b_s.a *= -2;
    b_r.c ^ ~b_c.b; }) ) );

  ASSERT(-2, b_r.a);
  ASSERT(4, b_r.b);
  ASSERT(-2, b_r.c);

  ASSERT(-4, b_s.a);
  ASSERT(-3, b_s.b);
  ASSERT(5, b_s.c);
}

int main() {
  bitint_bitfiled();
  bitint_bitfiled_var(-3, 2, -1);

  printf("OK\n");
}
