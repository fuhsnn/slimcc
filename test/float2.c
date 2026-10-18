#include "test.h"

long double g0 = 343.4314;

int main(void) {
  long double l0 = 343.4314;
  long double l1 = 343.4314L;

  ASSERT(0, g0 == l1);
  ASSERT(0, l0 == l1);

  ASSERT(0, 0.0/0.0 == 0.0/0.0);
  ASSERT(1, 0.0/0.0 != 0.0/0.0);

  ASSERT(0, 0.0/0.0 < 0);
  ASSERT(0, 0.0/0.0 <= 0);
  ASSERT(0, 0.0/0.0 > 0);
  ASSERT(0, 0.0/0.0 >= 0);

  ASSERT(0, 0.0f/0.0f == 0.0f/0.0f);
  ASSERT(1, 0.0f/0.0f != 0.0f/0.0f);

  ASSERT(0, 0.0f/0.0f < 0);
  ASSERT(0, 0.0f/0.0f <= 0);
  ASSERT(0, 0.0f/0.0f > 0);
  ASSERT(0, 0.0f/0.0f >= 0);

  ASSERT(0, 0.0L/0.0L == 0.0L/0.0L);
  ASSERT(1, 0.0L/0.0L != 0.0L/0.0L);

  ASSERT(0, 0.0L/0.0L < 0);
  ASSERT(0, 0.0L/0.0L <= 0);
  ASSERT(0, 0.0L/0.0L > 0);
  ASSERT(0, 0.0L/0.0L >= 0);

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
  {
    volatile double z = 0.0;
    volatile double n = z / z;
    ASSERT(0, n == n);
    ASSERT(1, n != n);
    ASSERT(0, n < n);
    ASSERT(0, n > n);
    ASSERT(0, n <= n);
    ASSERT(0, n >= n);
    ASSERT(1, !(n == n));
    ASSERT(0, !(n != n));
    ASSERT(1, !(n < n));
    ASSERT(1, !(n > n));
    ASSERT(1, !(n <= n));
    ASSERT(1, !(n >= n));
  }
  {
#define ROUND_TEST(it, iv, ft, fv) \
    static_assert(iv == fv); \
    ASSERT(1, ({ volatile it I = iv; volatile ft F = fv; I == F; }));

    ROUND_TEST( int32_t, 16777217, float, 16777216.0f);
    ROUND_TEST(uint32_t, 16777217, float, 16777216.0f);
    ROUND_TEST( int64_t, 16777217, float, 16777216.0f);
    ROUND_TEST(uint64_t, 16777217, float, 16777216.0f);

    ROUND_TEST( int32_t, 33554431, float, 33554432.0f);
    ROUND_TEST(uint32_t, 33554431, float, 33554432.0f);
    ROUND_TEST( int64_t, 33554431, float, 33554432.0f);
    ROUND_TEST(uint64_t, 33554431, float, 33554432.0f);

    ROUND_TEST( int64_t, 9007199254740993, double, 9007199254740992.0);
    ROUND_TEST(uint64_t, 9007199254740993, double, 9007199254740992.0);

    ROUND_TEST( int64_t, 18014398509481983, double, 18014398509481984.0);
    ROUND_TEST(uint64_t, 18014398509481983, double, 18014398509481984.0);
  }

  printf("OK\n");
}
