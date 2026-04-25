#ifndef _PLAT_SYS_CDEFS_H
#define _PLAT_SYS_CDEFS_H

#if !defined( __GNUC__) && !defined(__GNUC_MINOR__)
# define __GNUC__ 4
# define __GNUC_MINOR__ 1
# define __SLIMCC_FAKE_GNUC_SYS_CDEFS__
#endif

#include_next <sys/cdefs.h>

#ifdef __SLIMCC_FAKE_GNUC_SYS_CDEFS__
# undef __SLIMCC_FAKE_GNUC_SYS_CDEFS__
# undef __GNUC__
# undef __GNUC_MINOR__
#endif

#endif
