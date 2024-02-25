#include "test.h"

#if defined __GNUC__ && !defined __clang__
#define FASSERT ASSERT
#else
#define FASSERT EASSERT
#endif

int main(void) {

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
