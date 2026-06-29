#ifndef _SLIMCC_LIMITS_H
#define _SLIMCC_LIMITS_H

#include_next <limits.h>

#if __STDC_VERSION__ >= 202311L && !defined(BITINT_MAXWIDTH)
#define BITINT_MAXWIDTH __BITINT_MAXWIDTH__
#endif

#endif
