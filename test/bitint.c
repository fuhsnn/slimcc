#include "test.h"

#define CHK_USUAL_CONV(A, B, C) \
  static_assert(_Generic((A)0+(B)0, C:1))

CHK_USUAL_CONV(_BitInt(2), _Bool, int);
CHK_USUAL_CONV(unsigned _BitInt(1), _Bool, int);

CHK_USUAL_CONV(_BitInt(2), unsigned _BitInt(1), _BitInt(2));
CHK_USUAL_CONV(_BitInt(2), unsigned _BitInt(2), unsigned _BitInt(2));

CHK_USUAL_CONV(_BitInt(31), uint32_t, uint32_t);
CHK_USUAL_CONV(unsigned _BitInt(31), int32_t, int32_t);
CHK_USUAL_CONV(unsigned _BitInt(31), uint32_t, uint32_t);

CHK_USUAL_CONV(_BitInt(31), int32_t, int32_t);
CHK_USUAL_CONV(_BitInt(31), uint32_t, uint32_t);
CHK_USUAL_CONV(unsigned _BitInt(31), int32_t, int32_t);
CHK_USUAL_CONV(unsigned _BitInt(31), uint32_t, uint32_t);

CHK_USUAL_CONV(_BitInt(32), int32_t, int32_t);
CHK_USUAL_CONV(_BitInt(32), uint32_t, uint32_t);
CHK_USUAL_CONV(unsigned _BitInt(32), int32_t, uint32_t);
CHK_USUAL_CONV(unsigned _BitInt(32), uint32_t, uint32_t);

CHK_USUAL_CONV(_BitInt(33), int32_t, _BitInt(33));
CHK_USUAL_CONV(_BitInt(33), uint32_t, _BitInt(33));
CHK_USUAL_CONV(unsigned _BitInt(33), int32_t, unsigned _BitInt(33));
CHK_USUAL_CONV(unsigned _BitInt(33), uint32_t, unsigned _BitInt(33));

CHK_USUAL_CONV(_BitInt(64), int64_t, int64_t);
CHK_USUAL_CONV(_BitInt(64), uint64_t, uint64_t);
CHK_USUAL_CONV(unsigned _BitInt(64), int64_t, uint64_t);
CHK_USUAL_CONV(unsigned _BitInt(64), uint64_t, uint64_t);

CHK_USUAL_CONV(_BitInt(65), int64_t, _BitInt(65));
CHK_USUAL_CONV(_BitInt(65), uint64_t, _BitInt(65));
CHK_USUAL_CONV(unsigned _BitInt(65), int64_t, unsigned _BitInt(65));
CHK_USUAL_CONV(unsigned _BitInt(65), uint64_t, unsigned _BitInt(65));

struct { _BitInt(33)i:31;} bitint_bitfield;
static_assert(_Generic(-bitint_bitfield.i, _BitInt(33):1));
static_assert(_Generic(bitint_bitfield.i + 0, _BitInt(33):1));

int main() {
  printf("OK\n");
}
