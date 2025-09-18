#ifndef _SLIMCC_MATH_H
#define _SLIMCC_MATH_H

#include_next <math.h>

#define NAN __builtin_math_constant_nanf()
#define INFINITY __builtin_math_constant_inff()

#endif
