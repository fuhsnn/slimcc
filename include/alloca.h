#ifndef __ALLOCA_H
#define __ALLOCA_H

#include <stddef.h>

extern void *alloca (size_t);

#ifdef alloca
#undef alloca
#endif

#define alloca(sz) __builtin_alloca(sz)

#endif
