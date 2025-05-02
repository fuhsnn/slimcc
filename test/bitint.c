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

int main() {
  bitint_bitfiled();

  printf("OK\n");
}
