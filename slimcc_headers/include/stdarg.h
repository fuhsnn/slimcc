#ifndef __STDARG_H
#define __STDARG_H

typedef __builtin_va_list va_list;

#if __STDC_VERSION__ >= 202311L
#define va_start(...) __builtin_c23_va_start(__VA_ARGS__)
#else
#define va_start(ap, last) __builtin_va_start(ap, last)
#endif

#define va_end(ap) __builtin_va_end(ap)

#define va_arg(ap, ty) __builtin_va_arg(ap, ty)

#define va_copy(dest, src) __builtin_va_copy(dest, src)

#define __GNUC_VA_LIST 1
typedef va_list __gnuc_va_list;

#endif
