#ifndef _SLIMCC_PLAT_STRING_H
#define _SLIMCC_PLAT_STRING_H

#include_next <string.h>

#ifdef _GNU_SOURCE
#define strdupa(_str) (strcpy(__builtin_alloca(strlen(_str) + 1), _str))
#endif

#endif
