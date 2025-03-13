#ifndef _PLAT_SYS_CDEFS_H
#define _PLAT_SYS_CDEFS_H

#define __compiler_membar()     __asm __volatile(" " : : : "memory")

#define __CC_SUPPORTS_SYMVER 1

#include_next <sys/cdefs.h>

#define __inline inline

#undef _Alignas
#undef _Alignof
#undef _Noreturn
#undef _Thread_Local

#endif
