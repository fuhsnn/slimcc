#ifndef __STDFLOAT_H
#define __STDFLOAT_H

#define DECIMAL_DIG 21
#define FLT_EVAL_METHOD 0 // C11 5.2.4.2.2p9
#define FLT_RADIX 2

#if defined(__has_include)
#if __has_include(<fenv.h>)
#include <fenv.h>
#define FLT_ROUNDS                         \
  ({                                       \
    int val = -1;                          \
    switch (fegetround()) {                \
      case FE_TOWARDZERO: val = 0; break;  \
      case FE_TONEAREST: val = 1; break;   \
      case FE_UPWARD: val = 2; break;      \
      case FE_DOWNWARD: val = 3; break;    \
    };                                     \
    val;                                   \
  })
#endif
#endif

#ifndef FLT_ROUND
#define FLT_ROUND 1
#endif

#define FLT_DIG 6
#define FLT_EPSILON 0x1p-23f
#define FLT_MANT_DIG 24
#define FLT_MAX 0x1.fffffep+127f
#define FLT_MAX_10_EXP 38
#define FLT_MAX_EXP 128
#define FLT_MIN 0x1p-126f
#define FLT_MIN_10_EXP (-37)
#define FLT_MIN_EXP (-125)
#define FLT_TRUE_MIN 0x1p-149f

#define DBL_DIG 15
#define DBL_EPSILON 0x1p-52
#define DBL_MANT_DIG 53
#define DBL_MAX 0x1.fffffffffffffp+1023
#define DBL_MAX_10_EXP 308
#define DBL_MAX_EXP 1024
#define DBL_MIN 0x1p-1022
#define DBL_MIN_10_EXP (-307)
#define DBL_MIN_EXP (-1021)
#define DBL_TRUE_MIN 0x1p-1074

#define LDBL_DIG 18
#define LDBL_EPSILON 0x1p-63L
#define LDBL_MANT_DIG 64
#define LDBL_MAX 0x1.fffffffffffffffep+16383L
#define LDBL_MAX_10_EXP 4932
#define LDBL_MAX_EXP 16384
#define LDBL_MIN 0x1p-16382L
#define LDBL_MIN_10_EXP (-4931)
#define LDBL_MIN_EXP (-16381)
#define LDBL_TRUE_MIN 0x1p-16445L

#endif
