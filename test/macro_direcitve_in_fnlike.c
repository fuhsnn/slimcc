
#define G(...)
G(
#define M1 33
#define M2 22
#define F(x, y) x * y * x##y
#define M3 11
#define M23 5
)

static int a = M1 + M2 + M3;
static int b =  F(M2, 3);

#ifdef G

#define H(x,y) y + x

static int c =
H(
#endif
M1
,
#ifdef F
F(M2, 3)
-
#endif

M3
#ifdef H
G(99)
)
#endif
;

#include "test.h"

int main(void) {
  ASSERT(66, a);
  ASSERT(330, b);
  ASSERT(352, c);

  printf("OK\n");
}
