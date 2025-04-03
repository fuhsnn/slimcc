#ifndef _PLAT_STDLIB_H
#define _PLAT_STDLIB_H

#include_next <stdlib.h>

#define alloca(sz) __builtin_alloca(sz)

#endif
