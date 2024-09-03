#include "test.h"

#if defined __GNUC__ && !defined __clang__
#define FASSERT ASSERT
#else
#define FASSERT EASSERT
#endif

long double g0 = 343.4314;

int main(void) {
  long double l0 = 343.4314;
  long double l1 = 343.4314L;

  ASSERT(0, g0 == l1);
  ASSERT(0, l0 == l1);

  FASSERT(0, 0.0/0.0 == 0.0/0.0);
  FASSERT(1, 0.0/0.0 != 0.0/0.0);

  FASSERT(0, 0.0/0.0 < 0);
  FASSERT(0, 0.0/0.0 <= 0);
  FASSERT(0, 0.0/0.0 > 0);
  FASSERT(0, 0.0/0.0 >= 0);

  FASSERT(0, 0.0f/0.0f == 0.0f/0.0f);
  FASSERT(1, 0.0f/0.0f != 0.0f/0.0f);

  FASSERT(0, 0.0f/0.0f < 0);
  FASSERT(0, 0.0f/0.0f <= 0);
  FASSERT(0, 0.0f/0.0f > 0);
  FASSERT(0, 0.0f/0.0f >= 0);

  FASSERT(0, 0.0L/0.0L == 0.0L/0.0L);
  FASSERT(1, 0.0L/0.0L != 0.0L/0.0L);

  FASSERT(0, 0.0L/0.0L < 0);
  FASSERT(0, 0.0L/0.0L <= 0);
  FASSERT(0, 0.0L/0.0L > 0);
  FASSERT(0, 0.0L/0.0L >= 0);

  ASSERT(1, ({ float f = 0.0f; (_Bool)(f/f); }));
  ASSERT(1, ({ double d = -0.0; (d/d)?1:0; }));
  ASSERT(1, ({ long double ld = 0.0L; int i = 0; if(ld/ld) i = 1; i; }));

  {
    float pos_z; pos_z = +0.0f;
    float neg_z; neg_z = -0.0f;
    ASSERT(1, pos_z == neg_z && memcmp(&pos_z, &neg_z, sizeof(float)));
    pos_z = -pos_z;
    ASSERT(1, pos_z == neg_z && !memcmp(&pos_z, &neg_z, sizeof(float)));
  }
  {
    double pos_z; pos_z = +0.0;
    double neg_z; neg_z = -0.0;
    ASSERT(1, pos_z == neg_z && memcmp(&pos_z, &neg_z, sizeof(double)));
    neg_z = -neg_z;
    ASSERT(1, pos_z == neg_z && !memcmp(&pos_z, &neg_z, sizeof(double)));
  }
  {
    long double pos_z; pos_z = +0.0L;
    long double neg_z; neg_z = -0.0L;
    ASSERT(1, pos_z == neg_z && memcmp(&pos_z, &neg_z, 10));
  }

  printf("OK\n");
}
