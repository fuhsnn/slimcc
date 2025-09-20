#ifndef __STDALIGN_H
#define __STDALIGN_H

#if __STDC_VERSION__ < 202311L
#define alignas _Alignas
#define alignof _Alignof
#define __alignas_is_defined 1
#define __alignof_is_defined 1
#endif

#endif
