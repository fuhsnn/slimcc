#ifndef __ALLOCA_H
#define __ALLOCA_H

#include <stddef.h>

extern void *alloca (size_t);

#define alloca(sz) __builtin_alloca(sz)

#endif
