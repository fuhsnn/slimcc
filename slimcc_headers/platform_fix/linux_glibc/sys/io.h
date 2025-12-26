#ifndef _SLIMCC_PLAT_SYS_IO_H
#define _SLIMCC_PLAT_SYS_IO_H

#include <features.h>

#ifndef __GNUC__
#define __GNUC__ 2
#define __SLIMCC_FAKE_GNUC_IN_SYS_IO_H__
#endif

#include_next <sys/io.h>

#ifdef __SLIMCC_FAKE_GNUC_IN_SYS_IO_H__
#undef __GNUC__
#undef __SLIMCC_FAKE_GNUC_IN_SYS_IO_H__
#endif

#endif
