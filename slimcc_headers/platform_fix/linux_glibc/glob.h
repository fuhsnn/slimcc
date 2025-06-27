#ifndef _PLAT_GLOB_H
#define _PLAT_GLOB_H

#ifndef __REDIRECT_NTH
#define __REDIRECT_NTH(name, prot, asm_name) name prot __asm__(#asm_name)
#define __SLIMCC_REDIRECT_NTH_REDEF
#endif

#ifndef __REDIRECT_NTHNL
#define __REDIRECT_NTHNL(name, prot, asm_name) name prot __asm__(#asm_name)
#define __SLIMCC_REDIRECT_NTHNL_REDEF
#endif

#include_next <glob.h>

#ifdef __SLIMCC_REDIRECT_NTH_REDEF
#undef __SLIMCC_REDIRECT_NTH_REDEF
#undef __REDIRECT_NTH
#endif

#ifdef __SLIMCC_REDIRECT_NTHNL_REDEF
#undef __SLIMCC_REDIRECT_NTHNL_REDEF
#undef __REDIRECT_NTHNL
#endif

#endif
