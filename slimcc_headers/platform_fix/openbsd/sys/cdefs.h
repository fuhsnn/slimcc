#ifndef _PLAT_SYS_CDEFS_H
#define _PLAT_SYS_CDEFS_H

#include_next <sys/cdefs.h>

#undef __inline
#define __returns_twice __attribute__((returns_twice))
#define __only_inline extern __inline __attribute__((__gnu_inline__))
#define __packed        __attribute__((__packed__))
#define __aligned(x)    __attribute__((__aligned__(x)))
#define __dso_public    __attribute__((__visibility__("default")))
#define __dso_hidden    __attribute__((__visibility__("hidden")))

#endif
