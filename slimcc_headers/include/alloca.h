#ifndef __ALLOCA_H
#define __ALLOCA_H

#ifdef __linux__
#include <stddef.h>

extern void *alloca (size_t);
#endif

#define alloca(sz) __builtin_alloca(sz)

#endif
