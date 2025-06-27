#ifndef _PLAT_SYS_CDEFS_H
#define _PLAT_SYS_CDEFS_H

#include_next <sys/cdefs.h>

# define __REDIRECT(name, proto, alias) name proto __asm__ (__ASMNAME (#alias))
#  define __REDIRECT_NTH(name, proto, alias) \
     name proto __asm__ (__ASMNAME (#alias)) __THROW
#  define __REDIRECT_NTHNL(name, proto, alias) \
     name proto __asm__ (__ASMNAME (#alias)) __THROWNL
# define __ASMNAME(cname)  __ASMNAME2 (__USER_LABEL_PREFIX__, cname)
# define __ASMNAME2(prefix, cname) __STRING (prefix) cname

#ifndef __REDIRECT_FORTIFY
#define __REDIRECT_FORTIFY __REDIRECT
#endif

#ifndef __REDIRECT_FORTIFY_NTH
#define __REDIRECT_FORTIFY_NTH __REDIRECT_NTH
#endif

#endif

