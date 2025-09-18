#ifndef _PLAT_SYS_CDEFS_H
#define _PLAT_SYS_CDEFS_H

#include_next <sys/cdefs.h>

#ifndef __REDIRECT
#define __REDIRECT(name, prot, asm_name) name prot __asm__(#asm_name)
#endif

#ifndef __REDIRECT_NTH
#define __REDIRECT_NTH(name, prot, asm_name) name prot __asm__(#asm_name)
#endif

#ifndef __REDIRECT_NTHNL
#define __REDIRECT_NTHNL(name, prot, asm_name) name prot __asm__(#asm_name)
#endif

#endif
